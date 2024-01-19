---
title:                "एक वेब पेज डाउनलोड करना"
html_title:           "Kotlin: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

वेब पेज को डाउनलोड करना मतलब होता है कि आप उस पेज की सामग्री को अपने कंप्यूटर पर ले आते हैं। प्रोग्रामर्स इसे ज्यादातर वेब साइटों का डाटा खोजने और उसे संग्रहित करने के लिए करते हैं।

## कैसे करें:

चलिए देखते हैं कि Python में वेब पेज को कैसे डाउनलोड किया जा सकता है:

```Python
import requests

def download_page(url):
    response = requests.get(url)
    if response.status_code == 200:
        return response.text
    return None

print(download_page("https://www.example.com"))
```

उपरोक्त कोड का आउटपुट वेब पेज की एचटीएमएल सामग्री होगी।

## गहराई में:

वेब पेजेज को डाउनलोड करने के बारे में थोड़ी और जानकारी:

1. ऐतिहासिक प्रासंगिकता: प्रारंभ में, वेब ब्राउज़र्स ने यह कार्य किया, लेकिन फिर प्रोग्रामर्स ने स्क्रिप्टिंग भाषाओं का इस्तेमाल करके यह कार्य बहुत आसान बना दिया।
2. विकल्प: Python के अलावा भी अन्य भाषाओं में वेब पेज डाउनलोड के लिए libraries मौजूद हैं। जैसे Ruby के लिए `open-uri`, Java के लिए `Jsoup` आदि।
3. कार्यान्वयन विवरण: `requests` library HTTP requests के लिए सरल और प्रभावी है और उत्तर में आने वाले HTML को एनकोड करने के लिए इस्तेमाल किया जाता है।

## देखें भी:

- Python की requests library का डॉक्युमेंटेशन: [https://requests.readthedocs.io](https://requests.readthedocs.io)
- अधिक विवरण के लिए Beautiful Soup ट्यूटोरियल: [https://www.crummy.com/software/BeautifulSoup/bs4/doc/](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- Web scraping tutorial: [https://realpython.com/python-web-scraping-practical-introduction/](https://realpython.com/python-web-scraping-practical-introduction/)