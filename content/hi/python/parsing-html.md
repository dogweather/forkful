---
title:                "ह्ट्मल पार्सिंग"
html_title:           "Python: ह्ट्मल पार्सिंग"
simple_title:         "ह्ट्मल पार्सिंग"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/parsing-html.md"
---

{{< edit_this_page >}}

## क्या है और क्यों?
पार्सिंग (parsing) एक तकनीक है जो वेब पेजों के डेटा को अनुमान लगाने और जानकारी को स्ट्रक्चर्ड फॉर्म में रखने में मदद करती है। प्रोग्रामर हमेशा वेबसाइट्स के साइट मैप तैयार करने में और वेब पर उपलब्ध डेटा को सुरक्षित और स्वस्थ ढंग से उपयोग करने में पार्सिंग का उपयोग करते हैं।

## कैसे करें:
```python
# आवश्यक मॉड्यूल इम्पोर्ट करें
from bs4 import BeautifulSoup
import requests

# HTML पेज को डाउनलोड करें
url = "https://www.example.com"
r = requests.get(url)
html_content = r.content

# BeautifulSoup ऑब्जेक्ट बनाएं
soup = BeautifulSoup(html_content, 'html.parser')

# चालू पेज में उपलब्ध सभी हैडिंग्स को प्रिंट करें
for heading in soup.find_all('h2'):
    print(heading.text)
```

आउटपुट:
```
उदाहरण पेज हैडर
उदाहरण शीर्षक
``` 

## गहराई में जाएँ:
पार्सिंग की शुरुआत साल १९६१ में हुई जब एसजेएमएल (स्टैंडर्ड जनरलाइज्ड टैगिंग भाषा) यूआरएल से लिंक करने के लिए यूआरएल से लिंक करने के लिए वेब पेज को प्रदर्शित करने के लिए उपयोग किया जाता था। हालांकि, आजकल पार्सिंग का उपयोग वेब क्रॉलिंग, साइट मैपिंग, डेटा स्क्रैपिंग और सीओ टूल्स जैसे कामों के लिए भी किया जाता है। हालांकि, पार्सिंग का जो लोकप्रिय तरीका है वह BeautifulSoup है, लेकिन इसके अलावा भी अन्य एल्गोरिदम्स और लाइब्रेरीज उपलब्ध हैं।

## अधिक जानकारी:
- [BeautifulSoup Documentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [Web Scraping with Python: Collecting More Data from the Modern Web](https://www.oreilly.com/library/view/web-scraping-with/9781491985564/)
- [Introducing the BeautifulSoup Python Package](https://realpython.com/beautiful-soup-web-scraper-python/)