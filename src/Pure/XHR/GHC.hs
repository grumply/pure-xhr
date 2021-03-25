{-# language TypeApplications #-}
module Pure.XHR.GHC where

import Pure.XHR.Utils

import Pure.Data.JSON
import Pure.Data.Txt

get :: FromJSON b => Txt -> IO (Either XHRError b)
get _ = pure (Left GHCNotSupported)

getWith :: FrmJSON b => Txt -> [(Txt,Txt)] -> IO (Either XHRError b)
getWith _ _ = pure (Left GHCNotSupported)

getRaw :: [(Txt,Txt)] -> Txt -> IO (Either XHRError Txt)
getRaw _ _ = pure (Left GHCNotSupported)

post :: (ToJSON a,FromJSON b) => Txt -> a -> IO (Either XHRError b)
post _ _ = pure (Left GHCNotSupported)

postWith :: (ToJSON a, FromJSON b) => Txt -> [(Txt,Txt)] -> a -> IO (Either XHRError b)
postWith _ _ _ = pure (Left GHCNotSupported)

postRaw :: Txt -> [(Txt,Txt)] -> Txt -> IO (Either XHRError Txt)
postRaw _ _ _ = pure (Left GHCNotSupported)

postForm :: FromJSON b => Txt -> [(Txt,Txt)] -> IO (Either XHRError b)
postForm _ _ = pure (Left GHCNotSupported)

postFormWith :: FromJSON b => Txt -> [(Txt,Txt)] -> [(Txt,Txt)] -> IO (Either XHRError b)
postFormWith _ _ _ = pure (Left GHCNotSupported)

postFormRaw :: Txt -> [(Txt,Txt)] -> [(Txt,Txt)] -> IO (Either XHRError Txt)
postFormRaw _ _ _ = pure (Left GHCNotSupported)

-- This implementation is far too expensive in terms of dependencies.
{-
import Control.Lens ((^.))
import Network.Wreq as Wreq

import Control.Exception

get :: FromJSON a => Txt -> IO (Either XHRError a)
get url = do
  rsp <- handle @SomeException (pure . Left) (Right <$> Wreq.get (fromTxt url))
  pure $
    case rsp of
      Left se -> Left (OtherError url se)
      Right r -> let code = r ^. responseStatus . statusCode in
        case code of
          _ | code >= 200 && code < 300 -> either (Left . ParseError url) Right (decodeBSEither $ r ^. responseBody)
            | otherwise                 -> Left (StatusError url code)

getRaw :: Txt -> IO (Either XHRError Txt)
getRaw url = do
  rsp <- handle @SomeException (pure . Left) (Right <$> Wreq.get (fromTxt url))
  pure $
    case rsp of
      Left se -> Left (OtherError url se)
      Right r -> let code = r ^. responseStatus . statusCode in
        case code of
          _ | code >= 200 && code < 300 -> Right (toTxt $ r ^. responseBody)
            | otherwise                 -> Left (StatusError url code)

post :: (ToJSON a,FromJSON b) => Txt -> a -> IO (Either XHRError b)
post url payload = do
  rsp <- handle @SomeException (pure . Left) (Right <$> Wreq.post (fromTxt url) (toJSON payload))
  pure $
    case rsp of
      Left se -> Left (OtherError url se)
      Right r -> let code = r ^. responseStatus . statusCode in
        case code of
          _ | code >= 200 && code < 300 -> either (Left . ParseError url) Right (decodeBSEither $ r ^. responseBody)
            | otherwise                 -> Left (StatusError url code)

postForm :: (FromJSON b) => Txt -> [(Txt,Txt)] -> IO (Either XHRError b)
postForm url payload = do
  rsp <- handle @SomeException (pure . Left) (Right <$> Wreq.post (fromTxt url) params)
  pure $
    case rsp of
      Left se -> Left (OtherError url se)
      Right r -> let code = r ^. responseStatus . statusCode in
        case code of
          _ | code >= 200 && code < 300 -> either (Left . ParseError url) Right (decodeBSEither $ r ^. responseBody)
            | otherwise                 -> Left (StatusError url code)
  where
    params = fmap (\(k,v) -> fromTxt k := v) payload

postFormRaw :: Txt -> [(Txt,Txt)] -> IO (Either XHRError Txt)
postFormRaw url payload = do
  rsp <- handle @SomeException (pure . Left) (Right <$> Wreq.post (fromTxt url) params)
  pure $
    case rsp of
      Left se -> Left (OtherError url se)
      Right r -> let code = r ^. responseStatus . statusCode in
        case code of
          _ | code >= 200 && code < 300 -> Right (toTxt $ r ^. responseBody)
            | otherwise                 -> Left (StatusError url code)
  where
    params = fmap (\(k,v) -> fromTxt k := v) payload
-}