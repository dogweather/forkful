---
title:                "HTML पार्स करना"
html_title:           "C++: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "Python"
category:             "Python"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/parsing-html.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTML पार्सिंग का मतलब होता है HTML डॉक्यूमेंट को विश्लेषित करना ताकि उसकी हायरार्की और इलेमेंट्स को समझा जा सके। प्रोग्रामर्स इसे करते हैं ताकि वे वेबपेज की जानकारी को खोज सकें, संशोधित कर सकें और व्यवस्थित कर सकें।

## कैसे करें:
Python में, आप BeautifulSoup लाइब्ररी का उपयोग करके HTML पार्स कर सकते हैं। नीचे उदाहरण है:

```Python
from bs4 import BeautifulSoup

html_doc = """<html><head><title>मेरा पहला वेबपेज</title></head>
<body><p>हेल्लो दुनिया.</p></body></html>"""

soup = BeautifulSoup(html_doc, 'html.parser')

print(soup.prettify())
```

उपरोक्त कोड का आउटपुट नीचे दिया गया है:

```Python
<html>
 <head>
  <title>
   मेरा पहला वेबपेज
  </title>
 </head>
 <body>
  <p>
   हेल्लो दुनिया.
  </p>
 </body>
</html>
```

## गहरा डाइव
इतिहास में, HTML पार्सिंग की पहली बार आवश्यकता तब पेश आई जब वेब सर्फिंग मनोहारी और प्रभावी होने लगा। BeautifulSoup के अलावा, आप lxml और html.parser जैसे अन्य टूल्स का भी उपयोग कर सकते हैं। BeautifulSoup सबसे आम और लोकप्रिय पसंद है क्योंकि यह अधिक सहज और लचीला है। 

## देखने वाले
अधिक जानकारी के लिए, निम्नलिखित संसाधनों की जांच करें:
- [BeautifulSoup Documentation](https://www.crummy.com/software/BeautifulSoup/bs4/doc/)
- [Python's html.parser](https://docs.python.org/3/library/html.parser.html)
- [Parsing HTML using lxml](https://lxml.de/parsing.html)