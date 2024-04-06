---
date: 2024-01-20 18:00:21.547613-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902? (How to:) HTTP \u0905\
  \u0928\u0941\u0930\u094B\u0927 (Requests) 1990 \u0915\u0947 \u0926\u0936\u0915 \u0938\
  \u0947 \u0939\u0948\u0902 \u0914\u0930 \u0935\u0947\u092C \u0915\u0940 \u0928\u0940\
  \u0902\u0935 \u0939\u0948\u0902\u0964 `requests` \u092E\u0949\u0921\u094D\u092F\u0942\
  \u0932 \u092A\u093E\u092F\u0925\u0928 \u092E\u0947\u0902 HTTP \u0905\u0928\u0941\
  \u0930\u094B\u0927 \u0906\u0938\u093E\u0928\u0940 \u0938\u0947 \u0915\u0930\u0928\
  \u0947 \u0915\u0947 \u0932\u093F\u090F \u092C\u0928\u0940 \u0939\u0948\u0964\u2026"
lastmod: '2024-04-05T22:51:06.277602-06:00'
model: gpt-4-1106-preview
summary: ") HTTP \u0905\u0928\u0941\u0930\u094B\u0927 (Requests) 1990 \u0915\u0947\
  \ \u0926\u0936\u0915 \u0938\u0947 \u0939\u0948\u0902 \u0914\u0930 \u0935\u0947\u092C\
  \ \u0915\u0940 \u0928\u0940\u0902\u0935 \u0939\u0948\u0902\u0964 `requests` \u092E\
  \u0949\u0921\u094D\u092F\u0942\u0932 \u092A\u093E\u092F\u0925\u0928 \u092E\u0947\
  \u0902 HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u0906\u0938\u093E\u0928\u0940\
  \ \u0938\u0947 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u092C\u0928\
  \u0940 \u0939\u0948\u0964 \u0907\u0938\u0915\u0947 \u0935\u093F\u0915\u0932\u094D\
  \u092A \u092E\u0947\u0902 `http.client` \u0914\u0930 `urllib` \u091C\u0948\u0938\
  \u0947 \u0905\u0928\u094D\u092F \u092E\u0949\u0921\u094D\u092F\u0942\u0932 \u092D\
  \u0940 \u0936\u093E\u092E\u093F\u0932 \u0939\u0948\u0902, \u092A\u0930 `requests`\
  \ \u0905\u0927\u093F\u0915 \u0938\u0941\u0935\u093F\u0927\u093E\u091C\u0928\u0915\
  \ \u0939\u0948\u0964 \u0938\u0941\u0930\u0915\u094D\u0937\u093E, \u0932\u0949\u0917\
  \u093F\u0902\u0917, \u0914\u0930 \u0938\u0947\u0936\u0928 \u0939\u0948\u0902\u0921\
  \u0932\u093F\u0902\u0917 \u091C\u0948\u0938\u0940 \u0915\u093E\u0930\u094D\u092F\
  \u0915\u094D\u0937\u092E\u0924\u093E\u0913\u0902 \u0915\u093E \u0915\u094B\u0921\
  \ \u091C\u091F\u093F\u0932 \u0939\u094B \u0938\u0915\u0924\u093E \u0939\u0948, \u0907\
  \u0938\u0932\u093F\u090F \u0927\u094D\u092F\u093E\u0928 \u0938\u0947 \u092A\u094D\
  \u0930\u092F\u094B\u0917 \u0915\u0930\u0947\u0902\u0964."
title: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 44
---

## कैसे करें? (How to:)
```Python
import requests

# GET अनुरोध भेजें
response = requests.get('https://api.github.com')

# रिस्पांस को प्रिंट करें
print(response.status_code)  # स्टेटस कोड
print(response.json())      # JSON रिस्पांस को दिखाएँ

# POST अनुरोध उदाहरण
data = {'key': 'value'}
response = requests.post('https://httpbin.org/post', data=data)

# पोस्ट रिस्पांस देखें
print(response.json())
```

```
200
{'current_user_url': 'https://api.github.com/user', 'current_user_authorizations_html_url': 'https://github.com/settings/connections/applications{/client_id}', ... }
{'args': {}, 'data': '', 'files': {}, 'form': {'key': 'value'}, ... }
```

## डीप डाइव (Deep Dive)
HTTP अनुरोध (Requests) 1990 के दशक से हैं और वेब की नींव हैं। `requests` मॉड्यूल पायथन में HTTP अनुरोध आसानी से करने के लिए बनी है। इसके विकल्प में `http.client` और `urllib` जैसे अन्य मॉड्यूल भी शामिल हैं, पर `requests` अधिक सुविधाजनक है। सुरक्षा, लॉगिंग, और सेशन हैंडलिंग जैसी कार्यक्षमताओं का कोड जटिल हो सकता है, इसलिए ध्यान से प्रयोग करें।

## इस के साथ (See Also)
- `requests` के डॉक्यूमेंटेशन: https://requests.readthedocs.io/en/master/
- Python के आधिकारिक `http` मॉड्यूल डॉक्स: https://docs.python.org/3/library/http.html
- RESTful APIs के लिए मार्गदर्शक: https://www.restapitutorial.com/
- HTTP स्टेटस कोड्स: https://httpstatuses.com/
