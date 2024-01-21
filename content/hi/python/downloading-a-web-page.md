---
title:                "वेब पेज डाउनलोड करना"
date:                  2024-01-20T17:45:51.389845-07:00
model:                 gpt-4-1106-preview
simple_title:         "वेब पेज डाउनलोड करना"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/downloading-a-web-page.md"
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