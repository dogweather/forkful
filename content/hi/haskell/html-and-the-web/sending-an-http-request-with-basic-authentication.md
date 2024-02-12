---
title:                "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"
aliases:
- /hi/haskell/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:48.027702-07:00
model:                 gpt-4-1106-preview
simple_title:         "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
HTTP अनुरोध बेसिक प्रमाणीकरण के साथ एक सर्वर से संसाधन को एक्सेस करने के लिए credentials (उपयोगकर्ता नाम और पासवर्ड) भेजता है। प्रोग्रामर इसे सुरक्षित एपीआई या वेब सेवाओं तक प्राधिकृत पहुँच प्राप्त करने के लिए करते हैं।

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
