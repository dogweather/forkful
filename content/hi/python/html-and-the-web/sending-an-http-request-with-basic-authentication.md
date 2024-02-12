---
title:                "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"
aliases: - /hi/python/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:29.497709-07:00
model:                 gpt-4-1106-preview
simple_title:         "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों (What & Why)?

HTTP अनुरोध के साथ बेसिक प्रमाणीकरण का उपयोग करना यह सुनिश्चित करता है कि एपीआई या वेब सर्विस से संवेदनशील डेटा को सुरक्षित तरीके से एक्सेस किया जा सके। प्रोग्रामर्स इसे तब करते हैं जब उन्हें एक सुरक्षित API से जानकारी प्राप्त करनी होती है और उन्हें उपयोगकर्ता की पहचान और पासवर्ड के साथ प्रमाणित होना पड़ता है।

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
