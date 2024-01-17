---
title:                "एक http अनुरोध भेजना"
html_title:           "Python: एक http अनुरोध भेजना"
simple_title:         "एक http अनुरोध भेजना"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTTP अनुरोध भेजने का अर्थ है किसी वेब सर्वर से डेटा को आपके कम्प्यूटर या अन्य डिवाइस पर लाने का अनुरोध करना। यह प्रोग्रामर्स खुद वेब पेज देखने के स्थान पर रोबोट को पर्याप्त संख्या में सर्वर से डेटा लाने के लिए करते हैं।

## कैसे करें:
इस उदाहरण में, हम www.google.com से एक GET अनुरोध भेजेंगे और उसका जवाब प्राप्त करेंगे। हम अपने कोड में urllib और requests प्रकार का उपयोग करेंगे।

```python
import urllib
import requests

# GET अनुरोध भेजें
response = urllib.request.urlopen('http://www.google.com')

# जवाब की स्थिति परीक्षण करें
if response.code == 200:
    # डेटा प्राप्त करें
    data = response.read()
    print(data) # अब आप डिकोड्ड डेटा को प्रिंट कर सकते हैं।
else:
    print('अनुरोध विफल हुआ।')
```

आउटपुट:
```
<HTTP data>
```

## डीप डाइव:
HTTP (HyperText Transfer Protocol) एक ट्रांसफर प्रोटोकॉल है जो दो डिजिटल डिवाइस आपस में कनेक्ट करता है। यह वेब और इंटरनेट के विकास में महत्वपूर्ण भूमिका निभाता है। अल्टर्नेटिव के रूप में, आप हैडर और डेटा की स्वरूपण पर ध्यान देने के लिए अन्य कड़ी हो सकते हैं। HTTP अनुरोध python के SFU कोड क्लास से संबंधित है।

## देखें:
- [Requests: HTTP for Humans](https://requests.readthedocs.io/en/master/)
- [urllib: URL handling modules](https://docs.python.org/3/library/urllib.html)