{-# language CPP #-}
module Pure.XHR (get,getRaw,post,postForm,postFormRaw,module Utils) where

#ifdef __GHCJS__
import Pure.XHR.GHCJS
#else
import Pure.XHR.GHC
#endif

import Pure.XHR.Utils as Utils

-- This implementation is geared towards in-browser use. To this end, 
-- performance in GHC has been sacrificed for performance in GHCJS by
-- defaulting to Txt payloads instead of ByteString.

-- This library focuses on JSON-based requests and responses.