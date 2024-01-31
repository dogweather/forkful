---
title:                "HTML पार्स करना"
date:                  2024-01-20T15:35:13.973358-07:00
html_title:           "Bash: HTML पार्स करना"
simple_title:         "HTML पार्स करना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/parsing-html.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTML पार्स करना मतलब वेब पेज के HTML कोड को प्रोसेस करके उसकी स्ट्रक्चर और सामग्री को समझना और उपयोग करना। प्रोग्रामर्स इसे डेटा एक्सट्रैक्ट करने, वेब स्क्रेपिंग, और वेबसाइट्स की टेस्टिंग के लिए करते हैं।

## कैसे?
Python में HTML पार्स करने के लिए `BeautifulSoup` एक प्रसिद्ध लाइब्रेरी है। इसके इस्तेमाल का उदाहरण नीचे दिया गया है:

```Python
from bs4 import BeautifulSoup

# साधारण HTML डॉक्यूमेंट
html_doc = """
<html>
<head>
    <title>मेरा पेज</title>
</head>
<body>
    <h1>मेरा शीर्षक</h1>
    <p>मेरी पहली पैराग्राफ.</p>
</body>
</html>
"""

# BeautifulSoup ऑब्जेक्ट बनाना
soup = BeautifulSoup(html_doc, 'html.parser')

# टाइटल टैग प्रिंट करना
print(soup.title)
# शीर्षक टेक्स्ट प्रिंट करना
print(soup.h1.string)
# पहले पैराग्राफ को प्रिंट करना
print(soup.p.text)
```

नमूना आउटपुट:
```
<title>मेरा पेज</title>
मेरा शीर्षक
मेरी पहली पैराग्राफ.
```

## गहराई से जानकारी
`BeautifulSoup` को Leonard Richardson ने डेवलप किया था। HTML और XML फाइल्स को पार्स करने के लिए यह बहुत लोकप्रिय हो गया है। `lxml` और `html.parser` जैसे अलग पार्सर्स का इस्तेमाल करके `BeautifulSoup` विभिन्न प्रकार की पार्सिंग क्षमताओं को प्रदान करता है।

हालांकि, यदि वेब पेज जावास्क्रिप्ट द्वारा हेवी जनरेटेड होते हैं, तो `BeautifulSoup` अकेले पर्याप्त नहीं होता। इस स्थिति में, `Selenium` जैसे टूल्स को अपनाया जाता है जो ब्राउजर के जरिए पेज लोड करके HTML प्राप्त कर सकते हैं।

## यह भी देखें
- [BeautifulSoup डॉक्यूमेंटेशन](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [lxml लाइब्रेरी](https://lxml.de/)
- [Selenium डॉक्यूमेंटेशन](https://selenium-python.readthedocs.io/)
- [Web स्क्रेपिंग गाइड](https://realpython.com/beautiful-soup-web-scraper-python/)
