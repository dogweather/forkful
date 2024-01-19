---
title:                "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
html_title:           "C#: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या & क्यों?

HTTP सत्यापन के साथ अनुरोध भेजना क्या है और क्यों प्रोग्रामर्स इसका उपयोग करते हैं, आइए जानते हैं। 
HTTP सत्यापन के साथ अनुरोध भेजना एक प्रक्रिया है जिसमें हम HTTP अनुरोध के माध्यम से वेबसर्वर से वार्तालाप करते हैं। यह एक सुरक्षित तरीका होता है वेबसर्वर से डेटा एक्सेस करने का, अक्सर उपयोगकर्ता नाम और पासवर्ड के साथ। 

## कैसे करें?

Python की "requests" library का उपयोग करके HTTP अनुरोध भेजने का कोडिंग उदाहरण देखते हैं।

```Python
import requests
from requests.auth import HTTPBasicAuth 

response = requests.get('https://mywebsite.com', auth=HTTPBasicAuth('user', 'pass')) 

print(response.status_code)
```

ऊपरी कोड पर चलाने से आपको अपने अनुरोध की स्थिति कोड मिलेगा। यदि सब कुछ ठिक है, तो आपको 200 प्राप्त होगा।

## गहराई में:

HTTP Basic Authentication निम्न प्रकार के कोड को लागू करने की प्राचीन तकनीक है, जिसका इस्तेमाल उन साइटों द्वारा किया जाता है जो प्रथम प्रवेश के समय सत्यापन करती हैं। विकल्प के रूप में, आप OAuth या जावा वेब टोकन (JWT) का उपयोग कर सकते हैं। भारी लोड संचालन की स्थिति में, HTTP अनुरोधों को क्लस्टर में वितरित किया जा सकता है।

## देखें भी :

HTTP सत्यापन के साथ अनुरोध भेजने के बारे में और जानने के लिए निम्नलिखित लिंक पर जाएं:

- Python Requests Library: [https://requests.readthedocs.io/en/latest/](https://requests.readthedocs.io/en/latest/)
- HTTP Basic Authentication: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- OAuth 2.0: [https://oauth.net/2/](https://oauth.net/2/)
- JSON Web Tokens: [https://jwt.io/introduction/](https://jwt.io/introduction/)