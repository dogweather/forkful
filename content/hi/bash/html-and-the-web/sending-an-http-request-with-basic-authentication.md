---
date: 2024-01-20 18:00:59.359577-07:00
description: "How to (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902): Bash \u092E\
  \u0947\u0902 HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092C\u0941\u0928\u093F\u092F\
  \u093E\u0926\u0940 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\u0930\u0923\
  \ \u0915\u0947 \u0938\u093E\u0925 \u092D\u0947\u091C\u0928\u0947 \u0915\u0947 \u0932\
  \u093F\u090F `curl` \u0915\u092E\u093E\u0902\u0921 \u0915\u093E \u0907\u0938\u094D\
  \u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u0964\
  ."
lastmod: '2024-03-13T22:44:52.625947-06:00'
model: gpt-4-1106-preview
summary: "Bash \u092E\u0947\u0902 HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092C\
  \u0941\u0928\u093F\u092F\u093E\u0926\u0940 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\
  \u0915\u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 \u092D\u0947\u091C\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F `curl` \u0915\u092E\u093E\u0902\u0921 \u0915\u093E\
  \ \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0924\u0947 \u0939\
  \u0948\u0902\u0964."
title: "\u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\
  \u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 45
---

## How to (कैसे करें):
Bash में HTTP अनुरोध बुनियादी प्रमाणीकरण के साथ भेजने के लिए `curl` कमांड का इस्तेमाल करते हैं।

```Bash
# Basic HTTP अनुरोध भेजने का उदाहरण
curl -u username:password http://example.com

# सक्सेसफुल रिस्पॉन्स:
# HTTP/1.1 200 OK
# content-type: application/json
# {
#   "message": "आपने सफलतापूर्वक प्रमाणीकरण किया है!"
# }
```

## Deep Dive (गहराई से जानकारी):
बुनियादी प्रमाणीकरण HTTP प्रोटोकॉल का एक सरल प्रमाणन तरीका है जो रिक्वेस्ट हेडर में 'Authorization' फील्ड में यूजरनेम और पासवर्ड को Base64 एनकोडेड फॉर्मेट में जोड़ता है। 1990 के दशक में जब HTTP 1.0 पेश किया गया था, तब से यह मौजूद है। हालांकि, यह सबसे सुरक्षित प्रमाणन विधि नहीं मानी जाती क्योंकि यह आसानी से डीकोड किया जा सकता है। इसलिए, अधिक सुरक्षित ऑल्टरनेटिव्स के तौर पर OAuth, API keys, या JWT (JSON Web Tokens) उपयोग किए जाते हैं। फिर भी, बुनियादी प्रमाणीकरण को सरल इंटरफेस और लेस सिक्योर एपीआई कॉल्स के लिए अक्सर इस्तेमाल किया जाता है।

## See Also (और देखें):
- [cURL Documentation](https://curl.se/docs/)
- [MDN Web Docs: Authorization](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
- [HTTP Basic Authentication - Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication)
