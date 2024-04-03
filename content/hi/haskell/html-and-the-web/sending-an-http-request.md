---
date: 2024-01-20 18:00:12.085013-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Haskell\
  \ \u092E\u0947\u0902 HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\
  \u0928\u0947 \u0915\u0947 \u0932\u093F\u090F `http-client` \u0914\u0930 `http-client-tls`\
  \ \u092A\u0948\u0915\u0947\u091C \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\
  \u093E\u0932 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964."
lastmod: '2024-03-13T22:44:52.399159-06:00'
model: gpt-4-1106-preview
summary: "Haskell \u092E\u0947\u0902 HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\
  \u0947\u091C\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F `http-client` \u0914\u0930\
  \ `http-client-tls` \u092A\u0948\u0915\u0947\u091C \u0915\u093E \u0907\u0938\u094D\
  \u0924\u0947\u092E\u093E\u0932 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\
  \u0902\u0964."
title: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 44
---

## How to: (कैसे करें:)
Haskell में HTTP अनुरोध भेजने के लिए `http-client` और `http-client-tls` पैकेज का इस्तेमाल कर सकते हैं।

```Haskell
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.ByteString.Lazy.Char8 as L8

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    request <- parseRequest "http://httpbin.org/get"
    response <- httpLbs request manager
    L8.putStrLn $ responseBody response
```

जब आप ऊपर दी गई Haskell स्क्रिप्ट चलाएंगे, आपको `httpbin.org` से JSON रेस्पॉन्स मिलेगा जैसे:

```
{
  "args": {}, 
  "headers": {
    "Accept-Encoding": "gzip", 
    "Host": "httpbin.org", 
    ...
  }, 
  "origin": "xx.xx.xx.xx", 
  "url": "https://httpbin.org/get"
}
```

## Deep Dive (गहराई से जानकारी)
HTTP अनुरोध 1990 के दशक से वेब इंटरैक्शन की मूल अवधारणा है। `http-client` और `http-client-tls` Haskell की लाइब्रेरीज़ हैं जो कि सिक्योर (TLS/SSL) और साधारण HTTP अनुरोधों को हैंडल करती हैं। विकल्प के रूप में `Wreq` और `http-conduit` जैसे पैकेज भी हैं, लेकिन `http-client` संक्षिप्तता और नियंत्रण में बेहतर हो सकता है।

इसके अतिरिक्त, आप `Network.HTTP` मॉड्यूल का भी उपयोग कर सकते हैं, जो सिम्पल HTTP पैकेज प्रदान करता है, लेकिन यह नए `http-client` API की तुलना में कम फीचर्ड है।

## See Also (यह भी देखें)
- HTTP client package documentation: [http-client on Hackage](https://hackage.haskell.org/package/http-client)
- TLS support for HTTP client: [http-client-tls on Hackage](https://hackage.haskell.org/package/http-client-tls)
- Alternative HTTP packages: [Wreq](https://www.serpentine.com/wreq/) और [http-conduit](https://hackage.haskell.org/package/http-conduit)
