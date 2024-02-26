---
date: 2024-01-20 18:00:21.547613-07:00
description: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E\
  \ \u0907\u0902\u091F\u0930\u0928\u0947\u091F \u092A\u0930 \u0938\u0930\u094D\u0935\
  \u0930 \u0938\u0947 \u091C\u093E\u0928\u0915\u093E\u0930\u0940 \u092E\u093E\u0901\
  \u0917\u0928\u0947 \u0915\u093E \u0924\u0930\u0940\u0915\u093E \u0939\u0948\u0964\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\
  \u0938\u0947 \u0921\u0947\u091F\u093E \u092A\u094D\u0930\u093E\u092A\u094D\u0924\
  \ \u0915\u0930\u0928\u0947, \u0935\u0947\u092C\u0938\u0930\u094D\u0935\u093F\u0938\
  \u0947\u091C \u0938\u0947 \u092C\u093E\u0924 \u0915\u0930\u0928\u0947, \u0914\u0930\
  \ APIs \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0928\u0947 \u0915\
  \u0947\u2026"
lastmod: '2024-02-25T18:49:48.868255-07:00'
model: gpt-4-1106-preview
summary: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E\
  \ \u0907\u0902\u091F\u0930\u0928\u0947\u091F \u092A\u0930 \u0938\u0930\u094D\u0935\
  \u0930 \u0938\u0947 \u091C\u093E\u0928\u0915\u093E\u0930\u0940 \u092E\u093E\u0901\
  \u0917\u0928\u0947 \u0915\u093E \u0924\u0930\u0940\u0915\u093E \u0939\u0948\u0964\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\
  \u0938\u0947 \u0921\u0947\u091F\u093E \u092A\u094D\u0930\u093E\u092A\u094D\u0924\
  \ \u0915\u0930\u0928\u0947, \u0935\u0947\u092C\u0938\u0930\u094D\u0935\u093F\u0938\
  \u0947\u091C \u0938\u0947 \u092C\u093E\u0924 \u0915\u0930\u0928\u0947, \u0914\u0930\
  \ APIs \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0928\u0947 \u0915\
  \u0947\u2026"
title: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
HTTP अनुरोध भेजना इंटरनेट पर सर्वर से जानकारी माँगने का तरीका है। प्रोग्रामर्स इसे डेटा प्राप्त करने, वेबसर्विसेज से बात करने, और APIs का उपयोग करने के लिए करते हैं।

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
