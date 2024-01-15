---
title:                "वेब पेज डाउनलोड करना"
html_title:           "Haskell: वेब पेज डाउनलोड करना"
simple_title:         "वेब पेज डाउनलोड करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्यों

वैसे तो हम हर रोज़ वेब पेज्स को देखते हैं, पर अगर आपको उनमें से कुछ स्पेशल इन्फ़ोर्मेशन को दूसरी जगह पर स्टोर करना या उपयोग करना हो तो क्या करें? तो इस लेख में हम इस समस्या का हल ढूंढने के लिए हैस्केल प्रोग्रामिंग के उपयोग को समझेंगे। 

## कैसे करें

अतिरिक्त लाइब्ररी की जरूरत न होने से हैस्केल एक कम्पैक्ट भाषा है जो वेबी एपीआई के साथ अच्छा काम करती है। आप वेब पेज को डाउनलोड करने के लिए निम्न एल्गोरिथ्म का उपयोग कर सकते हैं:
```haskell
import Network.HTTP
import System.IO
main = simpleHTTP (getRequest "www.example.com") >>= getResponseBody >>= putStrLn
```
इस कोड का आउटपुट "वहाँ जाने के लिए लंबवत इंतज़ार" हो सकता है, पर यह आपको डाउनलोड होने वाली पूर्ण HTML झलक देगा। 

## गहराई में जाएं

अधिकांश वेब पेज एचटीएमएल में होते हैं और आपके वेब पेज डाउनलोड करने के बाद आपको हैस्केल को एचटीएमएल पार्स करने दर्शान [[HTML Parsers | http://hackage.haskell.org/package/html-conduit]] की उपयोग कर सकते हैं। यह आपको समझदारी से अपनी डाउनलोड की गयी वेब पेज को खोजने और चुनने की अनुमति देगा। 

## और देखें

[हैस्केल लेखिका श्रेणी | https://wiki.haskell.org/Category:Haskell] [[आम उपयोगिता सकारात्मक | https://wiki.haskell.org/Positive_criticisms]] [स्वायत्त पैकेज मैनेजर | https://hackage.haskell.org/]