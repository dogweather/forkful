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

## क्यों
 
एचटीटीपी अनुरोध भेजने का काम आपके वेब एप्लिकेशन को यह जानने में मदद करता है कि उसके साथ कौन-से वेब सर्वर संवाद कर रहे हैं। यह सुनिश्चित करने के लिए उपयोगी हो सकता है कि आपके अनुरोध को दूसरे स्वाद को प्रतिलिपि पते की प्रतिलिपि के लिए लोकल प्राइवेट पोर्ट पर भेजा जाता है। आपको एक वेब पेज का विवरण देना होगा जो आप खोज सकते हैं जो आपने अपने कंप्यूटर को भेजा है।

## कैसे करें

```python
import requests # requests पैकेज आवश्यक
#GET अनुरोध
response = requests.get("https://www.google.com")
print(response.status_code) # प्रतिक्रिया कोड मुद्रण की
print(response.content) # प्रतिक्रिया सामग्री मुद्रण की
```
```python
#POST अनुरोध
payload = {'key1': 'value1', 'key2': 'value2'} # पोस्ट प्राचल्य के साथ प्राचल्य पैकेट तयर करता है
response = requests.post("https://www.example.com/post", data=payload) # पोस्ट प्राचल्य भेजें
print(response.status_code) # प्रतिक्रिया कोड मुद्रण की
print(response.content) # प्रतिक्रिया सामग्री मुद्रण की 
```
## गहराई में

एचटीटीपी अनुरोध का अनुभव बहुत सारे तरीकों से विस्तारित है। आप रिक्ति अनुभाग में आप अनुरोध की हेडर, कुकीज़, गुप्त पाठ को सामान्य अनुरोध में सामिल करने के लिए सिखा सकते हैं। इसके अलावा, आप भी POST और PUT अनुरोध को कैसे भेज सकते हैं और कैसे फ़ाइलों को बैच में अपलोड कर सकते हैं इसकी अधिक जानकारी प्राप्त कर सकते हैं। यह एक सहज और उपयोगी तरीका है अपने कोड में व