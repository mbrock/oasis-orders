{-# Language TemplateHaskell #-}
{-# Language OverloadedStrings #-}
{-# Language LambdaCase #-}

import EVM.Types
import EVM.Dapp
import qualified EVM.Solidity

import Control.Lens
import Data.Aeson.Lens
import Data.ByteString (ByteString)
import Data.DoubleWord (Word256, Word160)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Vector (Vector)
import Pipes
import Pipes.Group
import System.Directory (withCurrentDirectory)
import System.IO

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as BS16
import qualified Data.Vector as Vector
import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude as P

newtype Id = Id Word256
  deriving (Eq, Ord, Show)

data MarketEvent
  = Make Id (Word160, Word256) (Word160, Word256)
  | Take Id (Word160, Word256) (Word160, Word256)
  deriving Show

data Log = Log
  { _logData   :: !ByteString
  , _logTopics :: !(Vector ByteString)
  }
  deriving Show

makeLenses ''Log

grok :: ByteString -> Maybe Log
grok bs = do
  v <- bs ^? _Object
  let hex = fst . BS16.decode . BS.drop 2 . encodeUtf8
  let topics = v ^.. ix "topics" . _Array . traverse . _String
  Log <$> (v ^? ix "data" . _String . to hex)
      <*> pure (fmap hex (Vector.fromList topics))

main :: IO ()
main = do
  withFile "logs" ReadMode $ \handle ->
    runEffect $ do
      let x = concats (view PB.lines (PB.fromHandle handle))
                >-> P.map grok
                >-> P.map (\case Just (Log b _) -> BS.length b
                                 Nothing -> 0)
      n <- P.sum x
      liftIO (print n)

  withCurrentDirectory "maker-otc" $
    EVM.Solidity.readSolc "out/matching_market.sol.json" >>=
      \case
        Just (contracts, cache) ->
          -- oasis (dappInfo "." contracts cache)
          pure ()
        Nothing ->
          error "Oasis Solidity JSON error"

-- oasis :: DappInfo -> IO ()
-- oasis dapp = do
--   print "hello"
