---
title:                "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
html_title:           "C#: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

HTTP अनुरोध के साथ बुनियादी प्रमाणीकरण भेजना क्या है? यह एक विशेष तरीका है जिसमें एक प्रोग्राम सर्वर से जानकारी मांगता है, लेकिन उससे पहले उसे खुद को प्रमाणित करता है। प्रोग्रामर इसे क्यों करते हैं? यह सुरक्षित और प्रामाणिक डाटा एक्सेस की आवश्यकता होती है। 

## कैसे करें (How to)

यहां है कि आप Haskell में इसे कैसे कर सकते हैं। 

```Haskell
import Network.HTTP
import Network.Browser
import Network.URI

main = do
 resp <- Network.Browser.browse $ do
 setAllowBasicAuth True
 request $ Request {rqURI = uri, rqMethod = GET, rqHeaders = [], rqBody = ""}
 print resp
 where uri = fromJust $ parseURI "http://username:password@mysite.com"
```
उपयोगकर्तानाम और पासवर्ड को आपके प्रमाणीकरण की जरूरतों के साथ बदलें।

## गहन अध्ययन (Deep Dive)

1. ऐतिहासिक संदर्भ: इसमें HTTP Basic Authentication भेजने की विधि शामिल है जो 1990 के दशक में W3C द्वारा विकसित की गई थी। 
2. विकल्प: इससे अधिक सुरक्षित ओएसआईएमएल और Digest Access Authentication प्रमाणीकरण विधाएं भी हैं।
3. कार्यान्वयन विवरण: यह कार्यान्वयन पुनः प्रयोग करने योग्य क्लास तत्व तैयार करता है जिसे आप ब्राउज़र सेटिंग्स में प्रयोग कर सकते हैं। यह सुनिश्चित करता है कि प्रमाणीकरण हमेशा सेट किया जाता है जब भी आवश्यक हो।

## अधिक देखें (See Also)

अधिक आपके संदर्भ के लिए, निम्नलिखित संसाधनों की जांच करें:

 हैस्कल HTTP पैकेज डॉक्यूमेंटेशन: http://hackage.haskell.org/package/HTTP
 
 हैस्कल नेटवर्क ब्राउज़र पैकेज डॉक्यूमेंटेशन: http://hackage.haskell.org/package/network-browser

वेब प्राधिकरण के तकनीकी विवरण के लिए, आप W3C की मानकों की जांच कर सकते हैं।