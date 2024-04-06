---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 18:35:59.702745-06:00
description: "\u0995\u09BF\u09AD\u09BE\u09AC\u09C7: Python-\u098F\u09B0 `xml.etree.ElementTree`\
  \ \u09AE\u09A1\u09BF\u0989\u09B2 XML \u09A8\u09BF\u09DF\u09C7 \u0995\u09BE\u099C\
  \ \u0995\u09B0\u09BE\u09B0 \u099C\u09A8\u09CD\u09AF \u099F\u09C1\u09B2\u09B8 \u09B8\
  \u09B0\u09AC\u09B0\u09BE\u09B9 \u0995\u09B0\u09C7\u0964 XML \u09A1\u0995\u09C1\u09AE\
  \u09C7\u09A8\u09CD\u099F \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09C1\u09A8\
  ."
lastmod: '2024-04-05T21:53:51.633237-06:00'
model: gpt-4-0125-preview
summary: "Python-\u098F\u09B0 `xml.etree.ElementTree` \u09AE\u09A1\u09BF\u0989\u09B2\
  \ XML \u09A8\u09BF\u09DF\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE\u09B0 \u099C\
  \u09A8\u09CD\u09AF \u099F\u09C1\u09B2\u09B8 \u09B8\u09B0\u09AC\u09B0\u09BE\u09B9\
  \ \u0995\u09B0\u09C7\u0964 XML \u09A1\u0995\u09C1\u09AE\u09C7\u09A8\u09CD\u099F\
  \ \u09AA\u09BE\u09B0\u09CD\u09B8 \u0995\u09B0\u09C1\u09A8."
title: "XML \u098F\u09B0 \u09B8\u09BE\u09A5\u09C7 \u0995\u09BE\u099C \u0995\u09B0\u09BE"
weight: 40
---

## কিভাবে:
Python-এর `xml.etree.ElementTree` মডিউল XML নিয়ে কাজ করার জন্য টুলস সরবরাহ করে।

XML ডকুমেন্ট পার্স করুন:
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
    print(f'Title: {title}, Author: {author}')
```
নমুনা আউটপুট:
```
Title: Learning Python, Author: Mark Lutz
Title: Programming Python, Author: Mark Lutz
```

XML ডকুমেন্ট তৈরি করুন:
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

## গভীর ডুব:
XML ৯০-এর দশকের শেষে এসজিএমএলের একটি সরলীকৃত উপসেট হিসেবে তৈরি হয়েছিল, যা অনলাইনে ডাটা শেয়ার করতে সহজে ব্যবহার করার জন্য। ওয়েব ডাটার জন্য JSON-এর জনপ্রিয়তা বৃদ্ধি পেলেও, XML প্রতিষ্ঠানিক, কনফিগারেশন, এবং ওয়েব সার্ভিসগুলি (SOAP, RSS) এর মধ্যে জীবনব্যাপী থাকে।

`xml.etree.ElementTree` ছাড়া বিকল্পগুলি অন্তর্ভুক্ত করে `lxml` এবং `minidom`। `lxml` দ্রুততর এবং বৈশিষ্ট্য-সমৃদ্ধ, অন্যদিকে `minidom` একটি আরও "DOM-এর মত" XML ইন্টারফেস প্রদান করে। বেছে নেওয়ার সময়, ব্যবহারের সহজতা, কার্যক্ষমতা, এবং নির্দিষ্ট বৈশিষ্ট্যের প্রয়োজনীয়তা বিবেচনা করুন।

আন্তরিকভাবে, `ElementTree` একটি এলিমেন্ট ট্রি মডেল অপারেট করে, যেখানে XML ফাইলের প্রতিটি উপাদান একটি গাছের নোড। এটি সরল পথ অভিব্যক্তি এবং অনুসন্ধানের জন্য অনুমতি দেয়, যা XML ডেটার কাঠামো ন্যাভিগেট এবং ম্যানিপুলেট করতে সহজ করে তোলে।

## আরো দেখুন:
- Python `xml.etree.ElementTree` মডিউল: https://docs.python.org/3/library/xml.etree.elementtree.html
- `lxml`: https://lxml.de/
- W3Schools XML টিউটোরিয়াল: https://www.w3schools.com/xml/
- XML স্পেসিফিকেশন: https://www.w3.org/XML/
