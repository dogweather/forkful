---
date: 2024-01-26 04:35:23.191792-07:00
description: "\u05D0\u05D9\u05DA \u05DC: \u05DE\u05D5\u05D3\u05D5\u05DC `xml.etree.ElementTree`\
  \ \u05E9\u05DC \u05E4\u05D9\u05D9\u05EA\u05D5\u05DF \u05DE\u05E6\u05D9\u05E2 \u05DB\
  \u05DC\u05D9\u05DD \u05DC\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML. \u05DC\
  \u05E0\u05EA\u05D7 \u05DE\u05E1\u05DE\u05DA XML."
lastmod: '2024-03-13T22:44:38.675309-06:00'
model: gpt-4-0125-preview
summary: "\u05DE\u05D5\u05D3\u05D5\u05DC `xml.etree.ElementTree` \u05E9\u05DC \u05E4\
  \u05D9\u05D9\u05EA\u05D5\u05DF \u05DE\u05E6\u05D9\u05E2 \u05DB\u05DC\u05D9\u05DD\
  \ \u05DC\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML"
weight: 40
---

## איך ל:
מודול `xml.etree.ElementTree` של פייתון מציע כלים לעבודה עם XML.

לנתח מסמך XML:
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
תוצאת דוגמה:
```
Title: Learning Python, Author: Mark Lutz
Title: Programming Python, Author: Mark Lutz
```

ליצור מסמך XML:
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

## להיכנס לעובי הקורה:
XML קיים מאז שנות ה-90 המאוחרות, נוצר כתת-קבוצה מפושטת של SGML לשיתוף נתונים אונלייני בקלות. למרות הפופולריות הגוברת של JSON לנתוני רשת, XML נותר חיוני בתחומים רבים כמו עסקים, הגדרות, ושירותי רשת (SOAP, RSS).

חלופות ל-`xml.etree.ElementTree` כוללות את `lxml` ו-`minidom`. `lxml` מהיר יותר ועשיר יותר במאפיינים, בעוד `minidom` מספק ממשק XML בסגנון "DOM-like". בעת הבחירה, יש לשקול את נוחות השימוש, הביצועים, ודרישות המאפיינים הספציפיות.

מאחורי הקלעים, `ElementTree` פועל על דגם של עץ הרכיבים, שבו כל רכיב של קובץ ה-XML הוא צומת בעץ. זה מאפשר ביטויים וחיפושים של מסלול ישירים, והופך את הניווט והשינוי במבנה נתוני ה-XML לקלים יותר.

## ראה גם:
- מודול `xml.etree.ElementTree` של פייתון: https://docs.python.org/3/library/xml.etree.elementtree.html
- `lxml`: https://lxml.de/
- מדריך XML של W3Schools: https://www.w3schools.com/xml/
- מפרט XML: https://www.w3.org/XML/
