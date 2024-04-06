---
date: 2024-01-20 17:59:05.381085-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0906\u0907\
  \u090F `curl` \u0915\u092E\u093E\u0902\u0921 \u0915\u093E \u0907\u0938\u094D\u0924\
  \u0947\u092E\u093E\u0932 \u0915\u0930\u0915\u0947 \u090F\u0915 HTTP GET \u0905\u0928\
  \u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0924\u0947 \u0939\u0948\u0902."
lastmod: '2024-04-05T21:53:54.593448-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0906\u0907\u090F\
  \ `curl` \u0915\u092E\u093E\u0902\u0921 \u0915\u093E \u0907\u0938\u094D\u0924\u0947\
  \u092E\u093E\u0932 \u0915\u0930\u0915\u0947 \u090F\u0915 HTTP GET \u0905\u0928\u0941\
  \u0930\u094B\u0927 \u092D\u0947\u091C\u0924\u0947 \u0939\u0948\u0902."
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
