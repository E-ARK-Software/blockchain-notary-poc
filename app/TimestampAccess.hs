{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module TimestampAccess (writeHash, getTimestamp, TimestampInfo(TimestampInfo)) where

import Data.Bifunctor (bimap)
import Data.ByteArray.Sized (SizedByteArray, convert, sizedByteArray)
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import Data.Solidity.Prim.Address (toHexString)
import Data.String (fromString)
import Lens.Micro ((.~))
import Network.Ethereum (Address, UIntN, Account(withAccount), to, withParam, DefaultAccount)
import Network.Web3 (runWeb3, Web3)
import Network.Web3.Provider (Web3Error)

import Timestamp (addValue, timestamps)

data TimestampInfo = TimestampInfo {
  _hash :: ByteString,
  _timestamp :: Int,
  _creator :: String}

withAccountAndParam :: String -> DefaultAccount Web3 a -> Web3 a
withAccountAndParam contractAddress = withAccount () . withParam (to .~ fromString contractAddress)

getTimestamp :: String -> ByteString -> IO (Either String (Maybe TimestampInfo))
getTimestamp contractAddress hash = unifyResult (uncurry $ toTimestampInfo hash) $ runWeb3 .
  withAccountAndParam contractAddress . timestamps . convert <$> toSizedByteArray hash

writeHash :: String -> ByteString -> IO (Either String ByteString)
writeHash contractAddress hash = unifyResult (const hash) $
  runWeb3 . withAccountAndParam contractAddress . addValue . convert <$> toSizedByteArray hash

unifyResult :: (a -> b) -> Either String (IO (Either Web3Error a)) -> IO (Either String b)
unifyResult f = either (pure . Left) (fmap $ bimap show f)

toSizedByteArray :: ByteString -> Either String (SizedByteArray 32 ByteString)
toSizedByteArray a =
  maybe (Left $ "data has wrong size: " <> show (ByteString.length a)) Right $ sizedByteArray a

toTimestampInfo :: ByteString -> UIntN 256 -> Address -> Maybe TimestampInfo
toTimestampInfo hash t c = flip (TimestampInfo hash) (show $ toHexString c) <$> fromUInt t

fromUInt :: UIntN 256 -> Maybe Int
fromUInt = (\n -> if n==0 then Nothing else Just n) . fromInteger . toInteger
