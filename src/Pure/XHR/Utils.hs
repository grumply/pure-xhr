module Pure.XHR.Utils where

import Pure.Data.Txt

import Control.Exception

-- XHRErrors contain the target URL
data XHRError 
  = StatusError Txt Int
  | ParseError Txt String
  | InvalidURLError Txt Txt
  | OtherError Txt SomeException
  | GHCNotSupported

xhrErrorURL :: XHRError -> Txt
xhrErrorURL (StatusError     u _) = u
xhrErrorURL (ParseError      u _) = u
xhrErrorURL (InvalidURLError u _) = u
xhrErrorURL (OtherError      u _) = u