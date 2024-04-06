---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:13.195715-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E42\u0E21\u0E14\u0E39\
  \u0E25 `xml.etree.ElementTree` \u0E02\u0E2D\u0E07 Python \u0E19\u0E33\u0E40\u0E2A\
  \u0E19\u0E2D\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E21\u0E37\u0E2D\u0E43\u0E19\
  \u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML \u0E27\u0E34\
  \u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E40\u0E2D\u0E01\u0E2A\u0E32\u0E23 XML."
lastmod: '2024-04-05T21:54:01.211046-06:00'
model: gpt-4-0125-preview
summary: "\u0E42\u0E21\u0E14\u0E39\u0E25 `xml.etree.ElementTree` \u0E02\u0E2D\u0E07\
  \ Python \u0E19\u0E33\u0E40\u0E2A\u0E19\u0E2D\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\
  \u0E07\u0E21\u0E37\u0E2D\u0E43\u0E19\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\
  \u0E01\u0E31\u0E1A XML \u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E40\
  \u0E2D\u0E01\u0E2A\u0E32\u0E23 XML."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML"
weight: 40
---

## วิธีการ:
โมดูล `xml.etree.ElementTree` ของ Python นำเสนอเครื่องมือในการทำงานกับ XML

วิเคราะห์เอกสาร XML:
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
    print(f'ชื่อหนังสือ: {title}, ผู้เขียน: {author}')
```
ผลลัพธ์ตัวอย่าง:
```
ชื่อหนังสือ: Learning Python, ผู้เขียน: Mark Lutz
ชื่อหนังสือ: Programming Python, ผู้เขียน: Mark Lutz
```

สร้างเอกสาร XML:
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

## ลงลึก:
XML มีมาตั้งแต่ปลายปี '90s, ถูกสร้างขึ้นเป็นชุดย่อยที่ง่ายกว่าของ SGML เพื่อการแบ่งปันข้อมูลออนไลน์อย่างง่ายดาย แม้ JSON จะเป็นที่นิยมมากขึ้นสำหรับข้อมูลเว็บ, XML ยังคงเป็นสิ่งสำคัญในหลายๆ องค์กร, การกำหนดค่า, และบริการเว็บ (SOAP, RSS)

ทางเลือกที่แตกต่างจาก `xml.etree.ElementTree` รวมถึง `lxml` และ `minidom`. `lxml` เร็วและมีคุณสมบัติมากกว่า, ขณะที่ `minidom` ให้หน้าตาที่เหมือนกับ "DOM-like" สำหรับ XML ให้ความพิจารณาในเรื่องความง่ายในการใช้งาน, ประสิทธิภาพ, และความต้องการคุณสมบัติเฉพาะ

ลึกลงไป, `ElementTree` ทำงานบนโมเดลของต้นไม้องค์ประกอบ, ที่ซึ่งแต่ละส่วนประกอบของไฟล์ XML เป็นโหนดในต้นไม้ สิ่งนี้ทำให้มีการแสดงและค้นหาเส้นทางที่ตรงไปตรงมา, ทำให้ง่ายขึ้นในการนำทางและจัดการโครงสร้างข้อมูล XML

## ดูเพิ่มเติม:
- โมดูล Python `xml.etree.ElementTree`: https://docs.python.org/3/library/xml.etree.elementtree.html
- `lxml`: https://lxml.de/
- W3Schools บทเรียน XML: https://www.w3schools.com/xml/
- ข้อมูลจำเพาะ XML: https://www.w3.org/XML/
