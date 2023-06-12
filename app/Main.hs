{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import Control.Applicative (liftA2, liftA3)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, ToJSON(toJSON), object, KeyValue((.=)))
import Data.ByteArray.Encoding (Base(Base16), convertFromBase, convertToBase)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Ini.Config
import Data.Text (pack)
import Data.UUID.Types (fromString, UUID)
import Network.Wai
import Network.Wai.Handler.Warp
    ( setLogger, setPort, runSettings, defaultSettings )
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
import Network.Wai.Logger ( withStdoutLogger )

import Rio
import TimestampAccess (TimestampInfo(TimestampInfo))

type ObjectId = QueryParam "objectId" String
type LicenseHash = QueryParam "licenseHash" String
type Requester = QueryParam "requester" String
type ResultP = Post '[JSON] RioResult

type RioAPI =
  "registerObject" :> ObjectId :> ResultP :<|>
  "createLicense" :> LicenseHash :> ResultP :<|>
  "assignLicense" :> ObjectId :> LicenseHash :> ResultP :<|>
  "acceptLicense" :> Requester :> ObjectId :> LicenseHash :> ResultP :<|>
  "approveRequest" :> Requester :> ObjectId :> ResultP :<|>
  "timestamp" :> QueryParam "hash" String :> Get '[JSON] TimestampResult

newtype RioResult = RioResult (Either String ByteString)
newtype TimestampResult = TimestampResult (Either String (Maybe TimestampInfo))

instance ToJSON RioResult where
  toJSON :: RioResult -> Value
  toJSON (RioResult (Left err)) = object ["error" .= err]
  toJSON (RioResult (Right hash)) = object ["message" .=
    ("The information has been registered successfully under the hash " <>
    C8.unpack (convertToBase Base16 hash) <> ".")]

instance ToJSON TimestampResult where
  toJSON :: TimestampResult -> Value
  toJSON (TimestampResult (Left err)) = object ["error" .= err]
  toJSON (TimestampResult (Right Nothing)) =
    object ["message" .= ("The hash is not registered." :: String)]
  toJSON (TimestampResult (Right (Just (TimestampInfo hash t c)))) = object [
    "hash" .= C8.unpack (convertToBase Base16 hash),
    "timestamp" .= show t,
    "creator" .= c]

data Config = Config {
  contractAddress :: String,
  port :: Int} deriving (Eq, Show)

server :: Config -> Server RioAPI
server c = registerObjectH c :<|>
  createLicenseH c :<|>
  assignLicenseH c :<|>
  acceptLicenseH c :<|>
  approveRequestH c :<|>
  timestampH c

registerObjectH :: Config -> Maybe String -> Handler RioResult
registerObjectH c objectId = unifyResult RioResult $
  registerObject (contractAddress c) <$> objectIdP objectId

createLicenseH :: Config -> Maybe String -> Handler RioResult
createLicenseH c licenseHash = unifyResult RioResult $
  createLicense (contractAddress c) <$> hexHashP licenseHash

assignLicenseH :: Config -> Maybe String -> Maybe String -> Handler RioResult
assignLicenseH c objectId licenseHash = unifyResult RioResult $
  liftA2 (assignLicense $ contractAddress c) (objectIdP objectId) (hexHashP licenseHash)

acceptLicenseH :: Config -> Maybe String -> Maybe String -> Maybe String -> Handler RioResult
acceptLicenseH c requester objectId licenseHash = unifyResult RioResult $
  liftA3 (acceptLicense $ contractAddress c)
    (hexHashP requester) (objectIdP objectId) (hexHashP licenseHash)

approveRequestH :: Config -> Maybe String -> Maybe String -> Handler RioResult
approveRequestH c requester objectId = unifyResult RioResult $
  liftA2 (approveRequest $ contractAddress c) (hexHashP requester) (objectIdP objectId)

timestampH :: Config -> Maybe String -> Handler TimestampResult
timestampH c hash = unifyResult TimestampResult $
  timestamp (contractAddress c) <$> hexHashP hash

fromParameter :: Maybe String -> Either String String
fromParameter = maybe (Left "missing parameter") Right

objectIdP :: Maybe String -> Either String UUID
objectIdP a = fromParameter a >>= fromUUID

hexHashP :: Maybe String -> Either String ByteString
hexHashP a = fromParameter a >>= convertFromBase Base16 . C8.pack

unifyResult :: (Either String a -> b) -> Either String (IO (Either String a)) -> Handler b
unifyResult f a = liftIO $ f <$> either (pure . Left) id a

fromUUID :: String -> Either String UUID
fromUUID = maybe (Left "UUID not valid") Right . fromString

configParser :: IniParser Config
configParser = section "GENERAL" $
  liftA2 Config (fieldOf "contract" string) (fieldDefOf "port" number 8081)

rioAPI :: Proxy RioAPI
rioAPI = Proxy

app :: Config -> Application
app = logAllMiddleware . simpleCors . serve rioAPI . server

logAllMiddleware :: Application -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
logAllMiddleware a req r = do
    print $ requestMethod req
    print $ rawPathInfo req
    print $ requestHeaders req
    a req r

main :: IO ()
main = do
  configFile <- pack <$> readFile "config.ini"
  either putStrLn (\c -> runWithLogger (port c) $ app c) $ parseIniFile configFile configParser
  where
    runWithLogger p a = withStdoutLogger $ \logger ->
      runSettings (setPort p $ setLogger logger defaultSettings) a
