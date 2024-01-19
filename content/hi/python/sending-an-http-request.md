---
title:                "http अनुरोध भेजना"
html_title:           "Elixir: http अनुरोध भेजना"
simple_title:         "http अनुरोध भेजना"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/sending-an-http-request.md"
---

{{< edit_this_page >}}

Title: HTTP अनुरोध को पायथन में कैसे भेजें: एक गाइड

## क्या & क्यों?
HTTP अनुरोध भेजना इसका मतलब है कि आप एक वेब सर्वर से डेटा मांग रहे हैं या उसमें डेटा भेज रहे हैं। कार्यक्रमकर्ता इसे वेब में डेटा को प्राप्त और प्रसारित करने के लिए इस्तेमाल करते हैं।

## कैसे करें:
```Python
import requests
response = requests.get('https://www.example.com')
print(response.status_code)
```
ऊपरी कोड माइक्रोसॉफ्ट के वेबसाइट से HTTP अनुरोध भेजता है। यदि आपका अनुरोध सफल होता है, इसका मतलब है कि आपको 200 का स्थिति कोड दिखाई देगा।

## गहराई में:
HTTP अनुरोध का इतिहास 1991 में शुरू होता है जब तिम बर्नेर्स-ली ने इंटरनेट प्रोटोकॉल के रूप में इसकी विकास यात्रा शुरू की। इसके विकल्प में gRPC, GraphQL, और अन्य HTTP/2-आधारित प्रोटोकॉल शामिल हैं, जो कि HTTP/1.1 की तुलना में कई मामलों में बेहतर परिणाम देते हैं। जब पांच हाथ लाइब्रेरी HTTP अनुरोध भेजती है, तो यह वास्तव में आपके ब्राउज़र की तरह TCP/IP माध्यम से सर्वर से कनेक्ट हो जाती है।

## और भी देखें:
HTTP के बारे में अधिक जाने के लिए, आप निम्नलिखित लिंक की जांच कर सकते हैं। 
- Mozilla Developer Network पर HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP
- रिक्वेस्ट्स लाइब्रेरी डॉक्युमेंटेशन: http://docs.python-requests.org/en/latest/
- Real Python 'रिक्वेस्ट्स' पायथन लाइब्रेरी गाइड: https://realpython.com/python-requests/