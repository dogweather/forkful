---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:14.858574-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E21\u0E32\u0E43\u0E0A\
  \u0E49 REXML \u0E17\u0E35\u0E48\u0E23\u0E27\u0E21\u0E21\u0E32\u0E01\u0E31\u0E1A\
  \ Ruby \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\
  \u0E32\u0E30\u0E2B\u0E4C\u0E2A\u0E48\u0E27\u0E19\u0E1B\u0E23\u0E30\u0E01\u0E2D\u0E1A\
  \ XML \u0E01\u0E31\u0E19."
lastmod: '2024-03-17T21:57:56.752381-06:00'
model: gpt-4-0125-preview
summary: "\u0E21\u0E32\u0E43\u0E0A\u0E49 REXML \u0E17\u0E35\u0E48\u0E23\u0E27\u0E21\
  \u0E21\u0E32\u0E01\u0E31\u0E1A Ruby \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E41\u0E22\u0E01\
  \u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E2A\u0E48\u0E27\u0E19\u0E1B\
  \u0E23\u0E30\u0E01\u0E2D\u0E1A XML \u0E01\u0E31\u0E19."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML"
weight: 40
---

## วิธีการ:
มาใช้ REXML ที่รวมมากับ Ruby เพื่อแยกวิเคราะห์ส่วนประกอบ XML กัน:
```Ruby
require 'rexml/document'
include REXML

xml_data = <<-XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
XML

document = Document.new(xml_data)
document.elements.each('fruits/fruit') do |element|
  puts "Name: #{element.attributes['name']}, Color: #{element.attributes['color']}"
end
```
ผลลัพธ์:
```
Name: apple, Color: green
Name: banana, Color: yellow
```

การสร้าง XML ก็ทำได้ง่ายเช่นกัน:
```Ruby
doc = Document.new
doc.add_element 'fruits'
apple = doc.root.add_element 'fruit', {'name' => 'apple', 'color' => 'green'}
banana = doc.root.add_element 'fruit', {'name' => 'banana', 'color' => 'yellow'}
puts doc
```
ผลลัพธ์ XML:
```XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
```

## การศึกษาลึก:
รากฐานของ XML นั้นย้อนกลับไปในยุค 1990 เป็นชุดย่อยที่เรียบง่ายของ SGML สำหรับเอกสารเว็บ มันอาจจะเบิกบาน แต่มีโครงสร้างที่แน่นแฟ้น นั่นคือเหตุผลที่มันยังคงอยู่ Ruby มีหลายวิธีในการจัดการกับ XML REXML เป็นห้องสมุดทั้งหมดใน Ruby ที่เริ่มต้นได้ง่าย Nokogiri เป็นเจมที่ห่อหุ้มไลบรารีซีที่เร็วกว่า นำเสนอความเร็วและคุณสมบัติเพิ่มเติม การเลือกระหว่างพวกเขา? เริ่มต้นด้วย REXML สำหรับงานขนาดเล็กและย้ายไปที่ Nokogiri หากคุณต้องการพลังมากกว่านี้

ในหลังบ้าน, การแยกวิเคราะห์ XML เป็นเรื่องเกี่ยวกับการแปลประโยคเป็นโมเดล DOM หรือ SAX DOM สร้างต้นไม้ในหน่วยความจำ ขณะที่ SAX สตรีมเอกสารและปล่อยสัญญาณเมื่อมันแยกวิเคราะห์ REXML นำเสนอทั้งสองโมเดล แต่มักจะช้ากว่าการขยาย C ที่ใช้โดย Nokogiri

## ดูเพิ่มเติม:
- เอกสาร Ruby REXML: https://www.rubydoc.info/stdlib/rexml
- เจม Nokogiri: https://nokogiri.org/
- ข้อกำหนด XML: https://www.w3.org/XML/
- บทนำสู่ SAX: https://www.saxproject.org/
- การเปรียบเทียบ YAML กับ JSON กับ XML: https://www.upwork.com/resources/json-vs-xml-vs-yaml
