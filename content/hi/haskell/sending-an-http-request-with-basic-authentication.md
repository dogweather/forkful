---
title:                "आधारभूत प्रमाणीकरण के साथ एचटीटीपी अनुरोध भेजना"
html_title:           "Haskell: आधारभूत प्रमाणीकरण के साथ एचटीटीपी अनुरोध भेजना"
simple_title:         "आधारभूत प्रमाणीकरण के साथ एचटीटीपी अनुरोध भेजना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

क्या और क्यों?

HTTP अनुरोध संदेश में आधारभूत प्रमाणीकरण के साथ एक HTTP अनुरोध भेजना क्या है और क्यों करते हैं। एक HTTP अनुरोध भेजने से हम संचार करते हैं और सुरक्षित तरीके से डेटा को स्थानांतरित कर सकते हैं। यह प्रोग्रामर्स द्वारा उपयोग किया जाता है ताकि सुरक्षित डेटा ट्रांसफर हो सके।

कैसे करें:

```Haskell
import Network.HTTP.Simple

request <- parseRequest "http://www.example.com"
response <- withBasicAuth "username" "password" request
print $ getResponseBody response
```

आउटपुट:

```Haskell
"Response body"
```

गहराई तक:

अतीत संदर्भ, विकल्प और एचटीटीपी अनुरोध भेजने के साथ भेजने के लिए अनुरोध करने के तरीकों का संदर्भ दिया जाता है। आपको इस तरह की रिक्ति का उपयोग न करना चाहिए जो कोई अनुचित संदेश संदेश संदेश का प्रमाण द्वारा प्रोग्रामिंग करता है।

अनुरोध भेजने के लिए HTTP बेसिक प्रमाणीकरण का प्रमाण है। यह सुरक्षा स्तर का एक उच्चतम स्तर प्रदान करता है जो अनुरोध को सुरक्षित रूप से कॉल करता है। इसका उपयोग करने के लिए रिक्ति में अनुमोदन द्वारा अनुरोध प्रवेदन किया जाता है।

देखें भी:

- [HTTP सिमपल](https://hackage.haskell.org/package/http-simple)
- [एक स्थान पर पेशेवर ब्लॉग पोस्ट](https://arjuna-v.github.io/blog/extending-http-simple.html)
- [एचटीटीपी सुरक्षा प्रोटोकॉल विवरण](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)