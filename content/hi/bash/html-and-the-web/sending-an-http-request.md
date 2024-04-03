---
date: 2024-01-20 17:59:05.381085-07:00
description: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E\
  \ \u0935\u0947\u092C \u0938\u0930\u094D\u0935\u0930 \u0938\u0947 \u0938\u0942\u091A\
  \u0928\u093E \u092E\u093E\u0902\u0917\u0928\u093E \u0939\u0948\u0964 \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0947 \u0921\u0947\
  \u091F\u093E \u092A\u093E\u0928\u0947, \u0935\u0947\u092C \u0938\u0930\u094D\u0935\
  \u093F\u0938\u0947\u091C \u0915\u0947 \u0938\u093E\u0925 \u092C\u093E\u0924\u091A\
  \u0940\u0924 \u0915\u0930\u0928\u0947 \u0914\u0930 APIs \u0915\u093E \u0909\u092A\
  \u092F\u094B\u0917 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0915\
  \u0930\u0924\u0947 \u0939\u0948\u0902\u0964"
lastmod: '2024-03-13T22:44:52.620965-06:00'
model: gpt-4-1106-preview
summary: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E\
  \ \u0935\u0947\u092C \u0938\u0930\u094D\u0935\u0930 \u0938\u0947 \u0938\u0942\u091A\
  \u0928\u093E \u092E\u093E\u0902\u0917\u0928\u093E \u0939\u0948\u0964 \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0947 \u0921\u0947\
  \u091F\u093E \u092A\u093E\u0928\u0947, \u0935\u0947\u092C \u0938\u0930\u094D\u0935\
  \u093F\u0938\u0947\u091C \u0915\u0947 \u0938\u093E\u0925 \u092C\u093E\u0924\u091A\
  \u0940\u0924 \u0915\u0930\u0928\u0947 \u0914\u0930 APIs \u0915\u093E \u0909\u092A\
  \u092F\u094B\u0917 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0915\
  \u0930\u0924\u0947 \u0939\u0948\u0902\u0964."
title: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 44
---

## How to: (कैसे करें:)
आइए `curl` कमांड का इस्तेमाल करके एक HTTP GET अनुरोध भेजते हैं:

```Bash
# HTTP GET अनुरोध
curl https://api.example.com/data

# उदाहरण आउटपुट
{"name":"Example","description":"A sample response from the API."}
```

अब POST अनुरोध प्रयोग करते हैं:

```Bash
# HTTP POST अनुरोध
curl -d "param1=value1&param2=value2" -X POST https://api.example.com/data

# उदाहरण आउटपुट
{"status":"success","message":"Data was posted successfully."}
```

## Deep Dive (गहराई में जानकारी)
HTTP अनुरोध 1990 के दशक से वेब का आधार हैं। `curl` से पहले, Telnet या कस्टम स्क्रिप्ट्स का इस्तेमाल होता था। `wget`, `httpie` जैसे अल्टरनेटिव्स भी मौजूद हैं, लेकिन `curl` इसकी सादगी और व्यापकता के लिए प्रसिद्ध है। `curl` में अनुरोध मेथड, हेडर्स और डेटा को कस्टमाइज़ करने की क्षमता होती है, जो डेवलपर्स को परिष्कृत HTTP अनुरोध भेजने की अनुमति देती है।

## See Also (और जानकारी के लिए)
- `curl` मैनुअल: https://curl.se/docs/manpage.html
- HTTPie (एक विकल्प): https://httpie.io/
- `wget`: https://www.gnu.org/software/wget/manual/wget.html
