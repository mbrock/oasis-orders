{-# Language TemplateHaskell #-}
{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}

import Prelude hiding (log)

import EVM.ABI
import EVM.Types
import EVM.Dapp
import qualified EVM.Solidity

import Control.Lens
import Control.Monad ((>=>), when)
import Data.Aeson.Lens
import Data.Binary.Get (runGetOrFail)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.DoubleWord (Word256, Word160)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (Vector)
import Pipes
import System.Directory (withCurrentDirectory)
import System.Environment (getArgs)
import System.IO
import Text.Printf

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vector
import qualified Pipes.Prelude as Pipes
import qualified Pipes.Prelude.Text as Pipes

newtype Id = Id W256
  deriving (Eq, Ord, Show)

data Order
  = Make Id (Word160, Word256) (Word160, Word256)
  | Take Id (Word160, Word256) (Word160, Word256)
  | Spam Text
  deriving Show

data Log = Log
  { _logData   :: !ByteString
  , _logTopics :: !(Vector Word256)
  }
  deriving Show

makeLenses ''Log

parseTopic :: ByteString -> Maybe Word256
parseTopic bs =
  case runGetOrFail (getAbi (AbiUIntType 256)) (fromStrict bs) of
    Right ("", 32, AbiUInt 256 x) -> Just x
    e -> error (show e)

parseData :: ByteString -> [AbiType] -> Maybe (Vector AbiValue)
parseData bs ts =
  case runGetOrFail (getAbiSeq (length ts) ts) (fromStrict bs) of
    Right ("", _, vs) -> Just vs
    e -> error (show e)

grok :: Text -> Maybe Log
grok t = do
  v <- t ^? _Object
  let hex = fst . BS16.decode . BS.drop 2 . encodeUtf8
  let topics = v ^.. ix "topics" . _Array . traverse . _String
  Log <$> (v ^? ix "data" . _String . to hex)
      <*> (mapM parseTopic (fmap hex (Vector.fromList topics)))

decode :: DappInfo -> Log -> Maybe Order
decode dapp log = do
  topic0    <- preview (logTopics . ix 0) log
  eventType <- preview (dappEventMap . ix (W256 topic0)) dapp
  case eventType of
    Event "LogMake" _ _ -> do
      orderId <- preview (logTopics . ix 1 . to fromIntegral) log
      args <-
        parseData (view logData log)
          [AbiAddressType, AbiAddressType, AbiUIntType 128, AbiUIntType 128, AbiUIntType 64]
      payGemV <- preview (ix 0) args
      buyGemV <- preview (ix 1) args
      payAmtV <- preview (ix 2) args
      buyAmtV <- preview (ix 3) args
      case (payGemV, buyGemV, payAmtV, buyAmtV) of
        (AbiAddress payGem, AbiAddress buyGem, AbiUInt 128 payAmt, AbiUInt 128 buyAmt) ->
          pure $ Make (Id orderId) (payGem, payAmt) (buyGem, buyAmt)
        _ ->
          error "invalid order"
    Event "LogTake" _ _ -> do
      args <-
        parseData (view logData log)
          [ AbiUIntType 256
          , AbiAddressType
          , AbiAddressType
          , AbiUIntType 128
          , AbiUIntType 128
          , AbiUIntType 64
          ]
      orderIdV <- preview (ix 0) args
      payGemV  <- preview (ix 1) args
      buyGemV  <- preview (ix 2) args
      payAmtV  <- preview (ix 3) args
      buyAmtV  <- preview (ix 4) args
      case (orderIdV, payGemV, buyGemV, payAmtV, buyAmtV) of
        ( AbiUInt 256 orderId
          , AbiAddress payGem
          , AbiAddress buyGem
          , AbiUInt 128 payAmt
          , AbiUInt 128 buyAmt
          ) ->
          pure $ Take (Id (W256 orderId)) (payGem, payAmt) (buyGem, buyAmt)
        _ ->
          error "invalid order"
    Event s _ _ ->
      pure $ Spam s

type Market = Map Id ((Word160, Word256), (Word160, Word256))

applyOrder :: Market -> Order -> Market
applyOrder market =
  \case
    Make x pay buy ->
      Map.insert x (pay, buy) market
    Take x (payGem, payAmt1) (buyGem, buyAmt1) ->
      let
        f ((_, payAmt0), (_, buyAmt0)) =
          let
            payAmt2 = payAmt0 - payAmt1
            buyAmt2 = buyAmt0 - buyAmt1
          in
            if payAmt2 > 0 && buyAmt2 > 0
            then Just ((payGem, payAmt2), (buyGem, buyAmt2))
            else Nothing
      in
        Map.update f x market
    _ ->
      market

readMarket :: DappInfo -> Handle -> (Order -> Bool) -> IO Market
readMarket dapp logFile predicate =
  runEffect . Pipes.fold applyOrder mempty id $
    Pipes.fromHandleLn logFile
      >-> Pipes.mapFoldable (grok >=> decode dapp)
      >-> Pipes.filter predicate

isBuyOf :: Addr -> Order -> Bool
isBuyOf (Addr gem) =
  \case
    Make _ _ (x, _) -> x == gem
    Take _ _ (x, _) -> x == gem
    _ -> False

isSellOf :: Addr -> Order -> Bool
isSellOf (Addr gem) =
  \case
    Make _ (x, _) _ -> x == gem
    Take _ (x, _) _ -> x == gem
    _ -> False

showMarket :: Market -> IO ()
showMarket market = do
  let
    price ((_, x), (_, y)) = fromIntegral x / (fromIntegral y :: Float)
    orders = sortBy (comparing (price . snd)) (Map.toList market)
    f (Id i, ((_, x), (_, y))) =
      printf "0x%x %05.4f %05.4f %.4f\n"
        (fromIntegral i :: Integer)
        (fromIntegral y / 1e18 :: Double)
        (fromIntegral x / 1e18 :: Double)
        (fromIntegral x / fromIntegral y :: Double)

  mapM_ f orders

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 3) $
    error "usage: oasis dapp-path buy-gem sell-gem"
  let [dappPath, buyGem, sellGem] = args
  withFile "logs" ReadMode $ \handle -> do
    withCurrentDirectory dappPath $
      EVM.Solidity.readSolc "out/matching_market.sol.json" >>=
        \case
          Just (contracts, cache) -> do
            let dapp = dappInfo "." contracts cache
            market <-
              readMarket dapp handle
                (\x -> isBuyOf (read buyGem) x && isSellOf (read sellGem) x)
            liftIO (showMarket market)
          Nothing ->
            error "Oasis Solidity JSON error"
