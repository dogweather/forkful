---
title:                "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
html_title:           "Python: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# क्यों?

अगर आप एक वेब डेवलपर हैं और आपके पास कोई सुरक्षित साइट है तो आपको यूजर का अधिकारिक डेटा सुरक्षित रखने की जरूरत होती है। इसके लिए आप एक HTTP रिक्वेस्ट सुरक्षित रूप से भेज सकते हैं, जो आपके यूजर्स को सुरक्षित बनाता है।

# कैसे?

```Python
import requests

url = "https://example.com/api/users/1"
username = "user123"
password = "passw0rd"

response = requests.get(url, auth=(username, password))
print(response.status_code)
print(response.json())
```

इस कोड में, हम एक requests लाइब्रेरी का उपयोग करके एक HTTP रिक्वेस्ट भेज रहे हैं। इसमें हमने `auth` पैरामीटर का उपयोग करके बेसिक ऑथेंटिकेशन की जानकारी भी भेजी है। यह सुनिश्चित करता है कि हमारी रिक्वेस्ट सुरक्षित रूप से भेजी जाए।

# डीप डाइव

HTTP रिक्वेस्ट बेसिक ऑथेंटिकेशन का उपयोग सर्वर्स द्वारा यूजर्स को पहचानने के लिए किया जाता है। इसमें उपयोगकर्ता नाम और पासवर्ड भेज कर सर्वर द्वारा उपयोगकर्ता की पहचान की जाती है। यह एक सुरक्षित प्रमाणीकरण प्रोटोकॉल है और यह ब्लैक हैटिंग या मैन इन दमाओं से बचाता है।

# देखें भी

- [Using Basic Authentication in Python Requests library](https://www.geeksforgeeks.org/sending-get-request-using-python-requests-library/)
- [HTTP Basic Authentication](https://www.httpwatch.com/httpgallery/authentication/#header1)