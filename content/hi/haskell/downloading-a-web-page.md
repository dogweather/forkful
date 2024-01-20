---
title:                "एक वेब पेज डाउनलोड करना"
html_title:           "Kotlin: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या & क्यों?

वेब पेज को डाउनलोड करना मतलब है उसकी कॉपी करना अपने कम्प्यूटर में। कार्यक्रमकर्ता इसे करते हैं ताकि वे वेबसाइट की जानकारी का अनुप्रयोग कर सकें तथा विश्लेषण करने के लिए।

## कैसे करें:

```Haskell
import Network.HTTP
import Network.URI

main :: IO()
main = do
    rsp <- Network.HTTP.simpleHTTP (getRequest "http://www.example.com")
    body <- getResponseBody rsp
    putStrLn body
```

इस कोड की सहायता से आप "www.example.com" वेबसाइट को डाउनलोड कर सकते हैं। इसके आउटपुट में वेबसाइट की HTML बॉडी दिखेगी।

## गहराी में:

वेब पेज को डाउनलोड करने के लिए हम HTTP GET अनुरोध का उपयोग करते हैं, जो HTTP प्रोटोकॉल का हिस्सा है। यह प्रोटोकॉल 1990 के दशक के शुरुआत में बनाया गया था। 

वैकल्पिक तरीके जैसे कि Web Scraping और API calls भी हैं, हालांकि वे अधिक स्पष्ट तत्वों के लिए हैं। 

Haskell में, Network.HTTP का उपयोग करके, हम HTTP GET अनुरोध को इम्प्लीमेंट कर सकते हैं। यह पैकेज एक तरह का HTTP अनुरोध भेजने का तरीका प्रदान करता है।

## यह भी देखें:

1. Network.HTTP [डॉक्युमेंटेशन](https://hackage.haskell.org/package/HTTP)
2. Haskell प्रोग्रामिंग [गाइड](https://www.haskell.org/tutorial/)
3. Haskell के [Network.URI](https://hackage.haskell.org/package/network-uri-2.6.3.0/docs/Network-URI.html) पैकेज का विवरण