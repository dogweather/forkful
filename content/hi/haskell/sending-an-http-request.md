---
title:                "HTTP अनुरोध भेजना"
date:                  2024-01-20T18:00:12.085013-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP अनुरोध भेजना"

category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
HTTP अनुरोध भेजना यह सुनिश्चित करता है कि आपका प्रोग्राम इंटरनेट पर सर्वर से डेटा मंगवा सके। प्रोग्रामर्स इसे वेब एपीआई से संवाद, डाटा की प्राप्ति या भेजने और वेब सर्विसेज के सामान्य इंटरैक्शन के लिए करते हैं।

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
