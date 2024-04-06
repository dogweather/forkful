---
date: 2024-01-20 18:02:48.027702-07:00
description: "How to (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902): *\u0909\u0926\
  \u093E\u0939\u0930\u0923 \u0906\u0909\u091F\u092A\u0941\u091F:*."
lastmod: '2024-04-05T21:53:54.395322-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\
  \u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 45
---

## How to (कैसे करें):
```Haskell
{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Network.HTTP.Types.Header (hAuthorization)
import Network.HTTP.Base (encodeBase64)
import Data.ByteString.Char8 (pack)

-- | बेसिक प्रमाणीकरण के साथ HTTP GET अनुरोध भेजना
sendRequestWithBasicAuth :: IO ()
sendRequestWithBasicAuth = do
  manager <- newManager defaultManagerSettings
  let credentials = "username:password" -- यहाँ अपना उपयोगकर्ता नाम और पासवर्ड डालें
      authHeader = ("Authorization", "Basic " <> encodeBase64 (pack credentials))
      request = "http://example.com" -- अपना अनुरोध URL यहाँ डालें
  initialRequest <- parseRequest request
  let requestWithAuth = initialRequest { requestHeaders = [authHeader] }
  response <- httpLbs requestWithAuth manager
  putStrLn $ "Response status code: " ++ show (statusCode $ responseStatus response)
  print $ responseBody response

main :: IO ()
main = sendRequestWithBasicAuth
```
*उदाहरण आउटपुट:*
```
Response status code: 200
"सामग्री..."
```

## Deep Dive (गहन जानकारी):
HTTP बेसिक प्रमाणीकरण वेब के प्रारंभिक दिनों से है और सबसे सरल प्रमाणीकरण मेकानिज़म में से एक है। आप यूजरनेम और पासवर्ड को Base64 में एन्कोड करके मैनेजर कॉल के साथ अनुरोध हेडर में भेजते हैं। इसका विकल्प ओघ (OAuth) या एपीआई कुंजियों का उपयोग हो सकता है जो अधिक सुरक्षित होते हैं। फिर भी, बेसिक प्रमाणीकरण का उपयोग आंतरिक और कम सुरक्षा चिंता वाली सेवाओं में जारी है। Haskell में, http-client पैकेज नेटवर्क कॉल्स की आसानी से हैंडलिंग के लिए एक अधिक स्थूल तरीका प्रस्तुत करता है।

## See Also (देखें भी):
- [HTTP client package](https://hackage.haskell.org/package/http-client)
- [HTTP types for Haskell](https://hackage.haskell.org/package/http-types)
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [Hackage: Haskell package repository](https://hackage.haskell.org/)
