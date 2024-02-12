---
title:                "XML के साथ काम करना"
date:                  2024-01-26T04:35:48.819130-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/working-with-xml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
"XML के साथ काम करना" प्रोग्रामिंग का उपयोग करके XML (eXtensible Markup Language) फाइलों को पढ़ने, बनाने और संशोधित करने की प्रक्रिया को संदर्भित करता है। प्रोग्रामर ऐसा करते हैं क्योंकि XML डेटा विनिमय के लिए व्यापक रूप से उपयोग में है, इसकी प्लेटफॉर्म-स्वतंत्र प्रकृति और स्वयं-वर्णनात्मक प्रारूप के कारण।

## कैसे:
Python का `xml.etree.ElementTree` मॉड्यूल XML के साथ काम करने के लिए उपकरण प्रदान करता है।

एक XML दस्तावेज़ को पार्स करना:
```python
import xml.etree.ElementTree as ET

xml_data = """<?xml version="1.0"?>
<library>
    <book>
        <title>Learning Python</title>
        <author>Mark Lutz</author>
    </book>
    <book>
        <title>Programming Python</title>
        <author>Mark Lutz</author>
    </book>
</library>
"""

root = ET.fromstring(xml_data)
for book in root.findall('book'):
    title = book.find('title').text
    author = book.find('author').text
    print(f'शीर्षक: {title}, लेखक: {author}')
```
नमूना आउटपुट:
```
शीर्षक: Learning Python, लेखक: Mark Lutz
शीर्षक: Programming Python, लेखक: Mark Lutz
```

एक XML दस्तावेज़ बनाना:
```python
library = ET.Element('library')
book = ET.SubElement(library, 'book')
title = ET.SubElement(book, 'title')
title.text = 'Automate the Boring Stuff with Python'
author = ET.SubElement(book, 'author')
author.text = 'Al Sweigart'

tree = ET.ElementTree(library)
tree.write('library.xml')
```

## गहराई में:
XML 90 के दशक के अंत में ऑनलाइन डेटा साझाकरण के लिए एक सरलीकृत उपसेट के रूप में SGML का निर्माण किया गया था। वेब डेटा के लिए JSON की बढ़ती लोकप्रियता के बावजूद, XML कई एंटरप्राइज़, कॉन्फ़िगरेशन और वेब सेवाओं (SOAP, RSS) में महत्वपूर्ण है।

`xml.etree.ElementTree` के विकल्पों में `lxml` और `minidom` शामिल हैं। `lxml` तेज और अधिक विशेषता-समृद्ध है, जबकि `minidom` एक अधिक "DOM-समान" XML इंटरफ़ेस प्रदान करता है। चुनते समय, उपयोग में आसानी, प्रदर्शन, और विशिष्ट विशेषता आवश्यकताओं पर विचार करें।

मूल रूप से, `ElementTree` एक तत्व वृक्ष मॉडल पर काम करता है, जहां XML फाइल का प्रत्येक घटक एक वृक्ष में एक नोड है। यह सरल मार्ग व्यक्तियों और खोजों की अनुमति देता है, XML डेटा की संरचना को नेविगेट और संशोधित करना आसान बनाता है।

## देखें भी:
- Python `xml.etree.ElementTree` मॉड्यूल: https://docs.python.org/3/library/xml.etree.elementtree.html
- `lxml`: https://lxml.de/
- W3Schools XML ट्यूटोरियल: https://www.w3schools.com/xml/
- XML विशिष्टता: https://www.w3.org/XML/
