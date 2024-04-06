---
date: 2024-01-20 18:02:29.497709-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902 (How to): \u0938\u0948\
  \u0902\u092A\u0932 \u0906\u0909\u091F\u092A\u0941\u091F."
lastmod: '2024-04-05T21:53:53.613424-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\
  \u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 45
---

## कैसे करें (How to):
```python
import requests
from requests.auth import HTTPBasicAuth

url = "https://example.com/api"
username = "user1"
password = "user1password"

response = requests.get(url, auth=HTTPBasicAuth(username, password))

print(response.status_code)
print(response.content)
```

सैंपल आउटपुट:
```
200
b'{"data": "Here is your secure data!"}'
```

## गहराई से जानकारी (Deep Dive):
बेसिक प्रमाणीकरण HTTP प्रोटोकॉल का एक पार्ट है। यह बहुत पुराना तरीका है और सादगी इसकी मुख्य विशेषता है। यह बेस-64 एन्कोडिंग का उपयोग करता है – लेकिन यह एन्क्रिप्टेड नहीं होता है, इसलिए इसे SSL/TLS के साथ उपयोग करना चाहिए। इसके अलावा, OAuth जैसे और भी जटिल और सुरक्षित विकल्प मौजूद हैं, लेकिन बेसिक प्रमाणीकरण का सरलता और व्यापक समर्थन इसे आज भी प्रासंगिक बनाता है।

## और भी (See Also):
- Python `requests` library documentation: https://docs.python-requests.org/en/latest/
- HTTP Basic Auth (MDN web docs): https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Basic Auth with HTTPS (tutorial): https://realpython.com/python-requests/#authentication

इन लिंक्स के जरिए आप और जानकारी हासिल कर सकते हैं और अपने कोड को सुरक्षित और पावरफुल बना सकते हैं।
