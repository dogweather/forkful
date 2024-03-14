---
date: 2024-01-20 17:45:51.389845-07:00
description: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\
  \u094B\u0921 \u0915\u0930\u0928\u093E \u092E\u0924\u0932\u092C \u0907\u0902\u091F\
  \u0930\u0928\u0947\u091F \u0938\u0947 \u0921\u093E\u091F\u093E \u092A\u094D\u0930\
  \u093E\u092A\u094D\u0924 \u0915\u0930\u0928\u093E \u0939\u0948\u0964 \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u092F\u0947 \u0907\
  \u0938\u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0924\u093E\
  \u0915\u093F \u0935\u0947 \u0921\u093E\u091F\u093E \u0915\u093E \u0935\u093F\u0936\
  \u094D\u0932\u0947\u0937\u0923 \u0915\u0930 \u0938\u0915\u0947\u0902, \u0938\u094D\
  \u0935\u091A\u093E\u0932\u093F\u0924 \u092A\u0930\u093F\u092F\u094B\u091C\u0928\u093E\
  \u0913\u0902 \u0915\u0947 \u0932\u093F\u090F\u2026"
lastmod: '2024-03-13T22:44:51.589205-06:00'
model: gpt-4-1106-preview
summary: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E \u092E\u0924\u0932\u092C \u0907\u0902\u091F\u0930\
  \u0928\u0947\u091F \u0938\u0947 \u0921\u093E\u091F\u093E \u092A\u094D\u0930\u093E\
  \u092A\u094D\u0924 \u0915\u0930\u0928\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\
  \u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u092F\u0947 \u0907\u0938\
  \u0932\u093F\u090F \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0924\u093E\u0915\
  \u093F \u0935\u0947 \u0921\u093E\u091F\u093E \u0915\u093E \u0935\u093F\u0936\u094D\
  \u0932\u0947\u0937\u0923 \u0915\u0930 \u0938\u0915\u0947\u0902, \u0938\u094D\u0935\
  \u091A\u093E\u0932\u093F\u0924 \u092A\u0930\u093F\u092F\u094B\u091C\u0928\u093E\u0913\
  \u0902 \u0915\u0947 \u0932\u093F\u090F\u2026"
title: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
वेब पेज डाउनलोड करना मतलब इंटरनेट से डाटा प्राप्त करना है। प्रोग्रामर्स ये इसलिए करते हैं ताकि वे डाटा का विश्लेषण कर सकें, स्वचालित परियोजनाओं के लिए डाटा इकट्ठा कर सकें, या वेब अप्लीकेशन का परीक्षण कर सकें।

## How to: (कैसे करें:)
```Python
import requests

# URL से डेटा प्राप्त करने का उदाहरण
url = 'http://example.com'
response = requests.get(url)

# स्टेटस कोड और प्राप्त हुआ कंटेंट
print(f"Status Code: {response.status_code}")
print(f"Page Content:\n{response.text}")
```

सैंपल आउटपुट:
```
Status Code: 200
Page Content:
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
    <!-- और बहुत कुछ HTML कंटेंट यहाँ होगा -->
</head>
<body>
...
</body>
</html>
```

## Deep Dive (विस्तृत विवरण)
जब इंटरनेट नया नया आया था, वेब पेज डाउनलोड करने के लिए FTP जैसे उपकरण प्रयोग में आते थे। पर, आज HTTP प्रोटोकॉल वेब पेज एक्सेस के लिए मानक बन चुका है। 'requests' मॉड्यूल Python में HTTP अनुरोधों को संभालने के लिए सबसे लोकप्रिय है क्योंकि यह सरल और शक्तिशाली है। इसके विकल्प में urllib है, लेकिन 'requests' बेहतर हैंडलिंग और सिंटैक्स प्रदान करता है। जब वेबपेज डाउनलोड किया जाता है, तो स्टेटस कोड, हेडर्स, और कंटेंट जैसे विस्तृत डाटा प्राप्त किया जा सकता है, जो कि डेवलपर्स के लिए कई सारे परीक्षण और विश्लेषण कार्यों में उपयोगी होता है।

## See Also (और देखें)
- requests डॉक्यूमेंटेशन: [Requests: HTTP for Humans](https://docs.python-requests.org/en/master/)
- Python urllib मॉड्यूल: [urllib — URL handling modules](https://docs.python.org/3/library/urllib.html)
- वेब स्क्रेपिंग गाइड: [Beautiful Soup Documentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- HTTP स्टेटस कोड्स: [HTTP Status Codes](https://developer.mozilla.org/en-US/docs/Web/HTTP/Status)
