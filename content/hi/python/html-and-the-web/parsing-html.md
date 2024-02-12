---
title:                "HTML विश्लेषण"
date:                  2024-02-03T19:13:39.514405-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML विश्लेषण"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
HTML पार्सिंग एक वेबपेज के HTML कोड का विश्लेषण करने और विशिष्ट जानकारी या तत्वों को निकालने की प्रक्रिया है, जो वेब स्क्रैपिंग, डेटा माइनिंग या वेबसाइटों के साथ ऑटोमेशन इंटरैक्शन के लिए एक आम कार्य है। प्रोग्रामर इसे वेबसाइटों के साथ प्रोग्रामैटिकली इंटरैक्ट करने, डेटा निकालने, कार्यों को ऑटोमेट करने, या वेब अप्लिकेशंस का परीक्षण करने के लिए करते हैं।

## कैसे करें:
Python, वेब स्क्रैपिंग और HTML पार्सिंग के लिए BeautifulSoup और requests जैसी शक्तिशाली लाइब्रेरी प्रदान करता है। शुरुआत करने के लिए, यदि आपने पहले से इन लाइब्रेरीज को इंस्टॉल नहीं किया है, तो आपको उन्हें इंस्टॉल करना होगा:

```bash
pip install beautifulsoup4 requests
```

यहाँ एक वेबपेज का HTML कंटेन्ट प्राप्त करने के लिए `requests` का उपयोग करते हुए और इसे पार्स करने के लिए `BeautifulSoup` का इस्तेमाल करते हुए एक मूल उदाहरण दिया गया है:

```python
import requests
from bs4 import BeautifulSoup

# एक वेबपेज का कंटेन्ट प्राप्त करें
URL = 'https://example.com'
page = requests.get(URL)

# HTML कंटेन्ट को पार्स करें
soup = BeautifulSoup(page.content, 'html.parser')

# वेबपेज के शीर्षक को निकालने का उदाहरण
title = soup.find('title').text
print(f'वेबपेज शीर्षक: {title}')
```

**नमूना आउटपुट**:
```
वेबपेज शीर्षक: Example Domain
```

जैसे कि किसी वेबपेज से सभी लिंक्स निकालना, अधिक जटिल क्वेरीज़ के लिए, आप नेविगेट और पार्स ट्री की खोज करने के लिए BeautifulSoup के विभिन्न तरीकों का उपयोग कर सकते हैं:

```python
# <a> टैग्स के भीतर सभी लिंक्स निकालें
links = soup.find_all('a')

for link in links:
    href = link.get('href')
    print(href)
```

**नमूना आउटपुट**:
```
https://www.iana.org/domains/example
```

BeautifulSoup की लचीलापन आपको वेब सामग्री के साथ काम करने वाले प्रोग्रामरों के लिए HTML पार्सिंग को एक शक्तिशाली उपकरण बनाता है, जिससे आप आवश्यक डेटा की खोज को अनुकूलित कर सकते हैं।