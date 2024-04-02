---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:29.396399-06:00
description: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML\
  \ \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\
  \u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E41\u0E25\u0E30\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 XML \u0E43\u0E19 Swift \u0E42\
  \u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E2A\
  \u0E34\u0E48\u0E07\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\
  \u0E41\u0E25\u0E01\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25 \u0E42\u0E14\u0E22\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E40\u0E21\u0E37\u0E48\
  \u0E2D\u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E23\u0E27\u0E21\u0E40\u0E02\u0E49\
  \u0E32\u0E01\u0E31\u0E1A\u0E23\u0E30\u0E1A\u0E1A\u0E17\u0E35\u0E48 XML\u2026"
lastmod: '2024-03-17T21:57:56.584749-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML \u0E2B\
  \u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\
  \u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E41\u0E25\u0E30\u0E2A\u0E23\u0E49\u0E32\
  \u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25 XML \u0E43\u0E19 Swift \u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E2A\u0E34\u0E48\
  \u0E07\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E41\u0E25\
  \u0E01\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \ \u0E42\u0E14\u0E22\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E40\u0E21\u0E37\u0E48\u0E2D\
  \u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E23\u0E27\u0E21\u0E40\u0E02\u0E49\u0E32\
  \u0E01\u0E31\u0E1A\u0E23\u0E30\u0E1A\u0E1A\u0E17\u0E35\u0E48 XML\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML"
weight: 40
---

## อะไร & ทำไม?
การทำงานกับ XML หมายถึงการแยกวิเคราะห์และสร้างข้อมูล XML ใน Swift โปรแกรมเมอร์ทำสิ่งนี้เพื่อการแลกเปลี่ยนข้อมูล โดยเฉพาะเมื่อต้องการรวมเข้ากับระบบที่ XML เป็นรูปแบบมาตรฐาน

## วิธีการ:
Swift ให้บริการ `XMLParser` และ `XMLDocument` สำหรับการแยกวิเคราะห์ข้อมูล XML นี่คือส่วนของโค้ดสำหรับการแยกวิเคราะห์สตริง XML ง่ายๆ:

```swift
import Foundation

let xmlString = """
<?xml version="1.0" encoding="UTF-8"?>
<note>
    <to>Tove</to>
    <from>Jani</from>
    <heading>Reminder</heading>
    <body>Don't forget the party on Friday!</body>
</note>
"""

if let xmlData = xmlString.data(using: .utf8) {
    let parser = XMLParser(data: xmlData)
    parser.delegate = someParserDelegate // ตัวแทน XMLParserDelegate ของคุณ
    parser.parse()
}
```

คุณยังสามารถสร้าง XML โดยใช้ `XMLDocument`:

```swift
import Foundation

let note = XMLElement(name: "note")
let to = XMLElement(name: "to", stringValue: "Tove")
note.addChild(to)
let xmlDoc = XMLDocument(rootElement: note)

print(xmlDoc.xmlString(options: .nodePrettyPrint))
```

ตัวอย่างผลลัพธ์:

```xml
<note>
  <to>Tove</to>
</note>
```

## ลงลึก
XML หรือ Extensible Markup Language มีมาตั้งแต่ปลายยุค 90's มันเป็นรูปแบบที่กระชับแต่อ่านง่าย ทำให้มันเหมาะกับโครงสร้างข้อมูลที่ซับซ้อน Swift มีความสามารถในการแยกวิเคราะห์ XML ไม่เข้มข้นเหมือนกับที่พบใน ElementTree ของ Python หรือ JAXB ของ Java แต่ก็สามารถรับมือกับความต้องการพื้นฐานได้

ทางเลือกอย่าง JSON มักถูกนิยมในระบบใหม่ๆ เนื่องจากมีน้ำหนักเบาและตัวแยกวิเคราะห์ที่ไม่ซับซ้อน แต่ XML ยังคงมีความสำคัญในหลายระบบองค์กรและระบบเก่า

เมื่อทำงานกับ XML ใน Swift, `XMLParser` เป็นตัวแยกวิเคราะห์แบบสตรีมซึ่งหมายความว่ามันอ่านผ่านเอกสาร XML อย่างเรียงตามลำดับ สำหรับไฟล์ XML ขนาดใหญ่นี่เป็นวิธีที่ประหยัดหน่วยความจำ อย่างไรก็ตาม หากคุณกำลังมองหาความง่ายและข้อมูล XML ของคุณมีขนาดไม่ใหญ่มาก การใช้ `XMLDocument` อาจเป็นวิธีที่ง่ายกว่า

## ดูเพิ่มเติม
- [คู่มือการแยกวิเคราะห์ XML ของ Apple](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/XMLParsing/XMLParsing.html)
- [บทเรียน XML ของ W3Schools](https://www.w3schools.com/xml/)
