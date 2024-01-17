---
title:                "वर्तमान दिनांक प्राप्त करना"
html_title:           "Python: वर्तमान दिनांक प्राप्त करना"
simple_title:         "वर्तमान दिनांक प्राप्त करना"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
वर्तमान तारीख प्राप्त करना एक आम टास्क है जो कि यह बतता है कि आपको आपके कंप्यूटर पर कौन सा दिन है। साथ ही, क्योंकि यह एक प्रोग्रामिंग टास्क है, इसका उपयोग ध्यान रखने के लिए, रिपोर्ट करने के लिए या समय की सारणी तैयार करने के लिए किया जाता है।

## कैसे करें:
```Python
import datetime
today = datetime.datetime.now()
print(today.strftime("%d-%m-%Y"))
```

इस उदाहरण में, हमने ```datetime``` मॉड्यूल का उपयोग करके वर्तमान तारीख एक वेरिएबल में स्टोर किया है और फिर उसे हमारे इच्छित फॉर्मेट में प्रिंट किया है।

## गहराई समीक्षा:
(1) ऐतिहासिक संदर्भ:
वर्तमान तारीख प्राप्त करने के लिए सर्व प्रथम, किसी भी कंप्यूटर सिस्टम पर व्याख्यान भाषा (interpreter language) में से कई निर्मित टूल्स उपलब्ध थे। Python के साथ, हम इसका उपयोग कर सकते हैं जो कि भारतवर्ष और समय को शामिल करता है।

(2) विकल्प:
अधिक परिचित विकल्पों के रूप में, हम वर्तमान तारीख प्राप्त करने के लिए ```date``` और ```calendar``` मॉड्यूल का उपयोग कर सकते हैं जो Python के साथ शामिल हैं।

(3) अनुप्रयोग विवरण:
प्रोग्रामिंग में वर्तमान तारीख प्राप्त करने का सबसे अच्छा तरीका हमेशा इस्तेमाल करने वाले स्क्रिप्ट को आपके सिस्टम की सेटिंग्स पर निर्भर करता है। अन्यथा, आपको समय की गणना और सिस्टम समय की सामग्री के बीच को असमान्य तरीके से समायोजित कर आपको परेशानी हो सकती है।

## अन्य स्रोतों:
विस्तृत जानकारी और स्पेसिफिक उदाहरणों के लिए निम्नलिखित स्रोतों की जाँच करें:

- [Python Documentation for ```datetime```](https://docs.python.org/3/library/datetime.html)
- [Tutorial on Getting the Current Date in Python](https://www.programiz.com/python-programming/datetime/current-datetime)
- [DateTime module in Python (Hindi)](https://www.studytonight.com/python/datetime-in-python.php)

नोट: आपके सिस्टम की सेटिंग्स भिन्न हो सकती हैं और यदि आपको सामान्य समस्या होती है, तो कृपया अपने Python संस्करण परिसीमित समायोजन (configuration) करें।