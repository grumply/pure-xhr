{-# language OverloadedStrings #-}
module Pure.XHR.GHCJS (get,getWith,getRaw,post,postWith,postRaw,postForm,postFormWith,postFormRaw) where

import Pure.XHR.Utils

import Pure.Data.Lifted
import Pure.Data.JSON
import Pure.Data.Txt hiding (foldl')
import Pure.Data.URI

import Control.Concurrent
import Data.Foldable

newtype XHR = XHR JSV

foreign import javascript unsafe
  "$r = new XMLHttpRequest()"
    new_xhr_js :: IO XHR

foreign import javascript unsafe
  "$1.onreadystatechange = $2"
    on_ready_js :: XHR -> Callback (JSV -> IO ()) -> IO ()

foreign import javascript unsafe
  "$1.open('GET', $2, true)"
    open_get_js :: XHR -> Txt -> IO ()

foreign import javascript unsafe
  "$1.open('POST',$2,true)"
    open_post_js :: XHR -> Txt -> IO ()

foreign import javascript unsafe
  "$1.setRequestHeader($2,$3)"
    set_request_header_js :: XHR -> Txt -> Txt -> IO ()

foreign import javascript unsafe
  "$1.send()"
    send_js :: XHR -> IO ()

foreign import javascript unsafe
  "$1.send($2)"
    send_with_js :: XHR -> Txt -> IO ()

foreign import javascript unsafe
  "$r = $1.readyState"
    ready_js :: XHR -> IO Int
    
foreign import javascript unsafe
  "$r = $1.status"
    status_js :: XHR -> IO Int

foreign import javascript unsafe
  "$r = $1.responseText" 
    response_text_js :: XHR -> IO Txt

get :: FromJSON a => Txt -> IO (Either XHRError a)
get url = getWith url [("Content-Type","application/json"),("Accept","*/*")] 

getWith :: FromJSON a => Txt -> [(Txt,Txt)] -> IO (Either XHRError a)
getWith url headers = do
  ext <- getRaw headers url
  pure $
    case ext of
      Left e  -> Left e
      Right t -> either (Left . ParseError url) Right (decodeEither t)

getRaw :: [(Txt,Txt)] -> Txt -> IO (Either XHRError Txt)
getRaw headers url = do
  xhr <- new_xhr_js
  mv  <- newEmptyMVar
  cb  <- syncCallback1 ContinueAsync $ \_ -> do
    r <- ready_js xhr
    case r of
      4 -> do
        s <- status_js xhr
        case s of
          _ | s >= 200 && s < 300 -> do
              t <- response_text_js xhr
              putMVar mv (Right t)
            | otherwise -> 
              putMVar mv $ Left (StatusError url s)
      _ -> pure ()
  on_ready_js xhr cb
  open_get_js xhr url
  for_ headers $ \(h,v) -> set_request_header_js xhr h v
  send_js xhr
  ma <- takeMVar mv
  ma `seq` releaseCallback cb
  pure ma

post :: (ToJSON a, FromJSON b) => Txt -> a -> IO (Either XHRError b)
post url payload = postWith url [("Content-Type","application/json"),("Accept","application/json")] payload

postWith :: (ToJSON a, FromJSON b) => Txt -> [(Txt,Txt)] -> a -> IO (Either XHRError b)
postWith url headers payload = do
  ext <- postRaw url headers (encode payload)
  pure $
    case ext of
      Left e  -> Left e
      Right t -> either (Left . ParseError url) Right (decodeEither t)

postRaw :: Txt -> [(Txt,Txt)] -> Txt -> IO (Either XHRError Txt)
postRaw url headers payload = do
  xhr <- new_xhr_js
  mv  <- newEmptyMVar
  cb  <- syncCallback1 ContinueAsync $ \_ -> do
    r <- ready_js xhr
    case r of
      4 -> do
        s <- status_js xhr
        case s of
          _ | s >= 200 && s < 300 -> do
              t <- response_text_js xhr
              putMVar mv (Right t)
            | otherwise -> 
              putMVar mv $ Left (StatusError url s)
      _ -> pure ()
  on_ready_js xhr cb
  open_post_js xhr url
  for_ headers $ \(h,v) -> set_request_header_js xhr h v
  send_with_js xhr payload
  ma <- takeMVar mv
  ma `seq` releaseCallback cb
  pure ma

postForm :: FromJSON a => Txt -> [(Txt,Txt)] -> IO (Either XHRError a)
postForm url payload = postFormWith url [("Content-Type","application/x-www-form-urlencoded"),("Accept","application/json")] payload

postFormWith :: FromJSON a => Txt -> [(Txt,Txt)] -> [(Txt,Txt)] -> IO (Either XHRError a)
postFormWith url headers payload = do
  ext <- postFormRaw url headers payload
  pure $
    case ext of
      Left e  -> Left e
      Right t -> either (Left . ParseError url) Right (decodeEither t)

postFormRaw :: Txt -> [(Txt,Txt)] -> [(Txt,Txt)] -> IO (Either XHRError Txt)
postFormRaw url headers payload = do
  xhr <- new_xhr_js
  mv  <- newEmptyMVar
  cb  <- syncCallback1 ContinueAsync $ \_ -> do
    r <- ready_js xhr
    case r of
      4 -> do
        s <- status_js xhr
        case s of
          _ | s >= 200 && s < 300 -> do
              t <- response_text_js xhr
              putMVar mv (Right t)
            | otherwise -> 
              putMVar mv $ Left (StatusError url s)
      _ -> pure ()
  on_ready_js xhr cb
  open_post_js xhr url
  for_ headers $ \(h,v) -> set_request_header_js xhr h v
  send_with_js xhr params
  ma <- takeMVar mv
  ma `seq` releaseCallback cb
  pure ma
  where
    params = foldl' (\ps (k,v) -> ps <> "&" <> encodeURIComponent k <> "=" <> encodeURIComponent v) mempty payload

