---
title:                "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
html_title:           "Haskell: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्यों

एचटीटीपी अनुरोध भेजने के कारण में सबसे महत्वपूर्ण कारण है कि आप एक अन्य सर्वर से डेटा या संसाधनों को प्राप्त करना चाहते हैं जिसे basic authentication के साथ सुरक्षित होना चाहिए। इसके अलावा, आप एचटीटीपी क्योंकि वह प्रत्येक वेब एप्लिकेशन में उपयोग किया जाता है और यह आपको अन्य संबंधित कॉडिंग भाषाओं की ओर से अन्यान्य है।

## कैसे करें

इस कार्य में, हम दो भागों में विस्तार से सीखेंगे: पहले, एचटीटीपी कोड के साथ एक बेसिक ऑथेंटिकेशन अनुरोध भेजने का तरीका और दूसरा, उस अनुरोध का जवाब प्राप्त करने का तरीका। यह आपको एक अच्छा समझाने में मदद करेगा कि कैसे एचटीटीपी के साथ basic authentication का उपयोग करें।

### पहला भाग: अनुरोध भेजना

पहले, आपको हैस्केल कोड के साथ एक अन्य सर्वर से डेटा प्राप्त करने के लिए एचटीटीपी अनुरोध भेजने की आवश्यकता होती है। हम बेसिकएथेंटिकेशन नामक चरण प्रमाणीकरण उपयोग करके एक उपयोगकर्ता नाम और पासवर्ड के साथ अनुरोध भेजने का तरीका जानेंगे।

```
import Network.HTTP
import qualified Data.ByteString as BS

sendRequest :: IO BS.ByteString
sendRequest = do
  let username = "myusername"
  let password = "mypassword"
  let request = getRequest "http://www.example.com"
        `applyBasicAuth` username password
  simpleHTTP request >>= getResponseBody
```

ऊपर दिए गए कोड में, हमने Network.HTTP और Data.ByteString मॉड्यूल को इम्पोर्ट किया है जो प्रत्येक होता है कि हमें इस प्रोग्राम में आवश्यक है ह