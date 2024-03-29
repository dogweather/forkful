---
date: 2024-01-20 18:00:59.359577-07:00
description: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092C\u0941\u0928\u093F\u092F\
  \u093E\u0926\u0940 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\u0930\u0923\
  \ \u0915\u0947 \u0938\u093E\u0925 \u092D\u0947\u091C\u0928\u093E, \u090F\u0915 \u0935\
  \u0947\u092C \u0938\u0930\u094D\u0935\u0930 \u0915\u094B \u0909\u092A\u092F\u094B\
  \u0917\u0915\u0930\u094D\u0924\u093E \u0928\u093E\u092E \u0914\u0930 \u092A\u093E\
  \u0938\u0935\u0930\u094D\u0921 \u0915\u0947 \u0938\u093E\u0925 \u090F\u0915 \u0905\
  \u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u0947 \u0915\u0940 \u092A\
  \u094D\u0930\u0915\u094D\u0930\u093F\u092F\u093E \u0939\u0948\u0964 \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947\
  \ \u0935\u093F\u0936\u094D\u0935\u0938\u0928\u0940\u092F\u2026"
lastmod: '2024-03-13T22:44:52.625947-06:00'
model: gpt-4-1106-preview
summary: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092C\u0941\u0928\u093F\u092F\
  \u093E\u0926\u0940 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\u0930\u0923\
  \ \u0915\u0947 \u0938\u093E\u0925 \u092D\u0947\u091C\u0928\u093E, \u090F\u0915 \u0935\
  \u0947\u092C \u0938\u0930\u094D\u0935\u0930 \u0915\u094B \u0909\u092A\u092F\u094B\
  \u0917\u0915\u0930\u094D\u0924\u093E \u0928\u093E\u092E \u0914\u0930 \u092A\u093E\
  \u0938\u0935\u0930\u094D\u0921 \u0915\u0947 \u0938\u093E\u0925 \u090F\u0915 \u0905\
  \u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u0947 \u0915\u0940 \u092A\
  \u094D\u0930\u0915\u094D\u0930\u093F\u092F\u093E \u0939\u0948\u0964 \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947\
  \ \u0935\u093F\u0936\u094D\u0935\u0938\u0928\u0940\u092F\u2026"
title: "\u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\
  \u0927 \u092D\u0947\u091C\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
HTTP अनुरोध बुनियादी प्रमाणीकरण के साथ भेजना, एक वेब सर्वर को उपयोगकर्ता नाम और पासवर्ड के साथ एक अनुरोध भेजने की प्रक्रिया है। प्रोग्रामर्स इसे विश्वसनीय संसाधनों तक सुरक्षित पहुँच के लिए करते हैं।

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
