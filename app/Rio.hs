module Rio where

import Crypto.Hash (Digest, SHA3_256, hash)
import Data.ByteArray
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.UUID.Types as UUID
import Data.UUID.Types

import TimestampAccess (TimestampInfo, writeHash, getTimestamp)

registerObject :: String -> UUID -> IO (Either String ByteString)
registerObject c objectId = writeHash c $
  LByteString.toStrict (UUID.toByteString objectId) <> ByteString.replicate 16 0

createLicense :: String -> ByteString -> IO (Either String ByteString)
createLicense = writeHash

assignLicense :: String -> UUID -> ByteString -> IO (Either String ByteString)
assignLicense c objectId licenseHash = writeHash c . hexSha3 $
  toASCIIBytes objectId <> licenseHash

acceptLicense :: String -> ByteString -> UUID -> ByteString -> IO (Either String ByteString)
acceptLicense c requester objectId licenseHash = writeHash c . hexSha3 $
  requester <> toASCIIBytes objectId <> licenseHash

approveRequest :: String -> ByteString -> UUID -> IO (Either String ByteString)
approveRequest c requester objectId = writeHash c . hexSha3 $
  requester <> toASCIIBytes objectId

timestamp :: String -> ByteString -> IO (Either String (Maybe TimestampInfo))
timestamp = getTimestamp

hexSha3 :: ByteArrayAccess ba => ba -> ByteString
hexSha3 bs = convert (hash bs :: Digest SHA3_256)
