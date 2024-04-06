---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:33.494200-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E15\u0E31\u0E27\u0E2D\
  \u0E22\u0E48\u0E32\u0E07\u0E1C\u0E25\u0E25\u0E31\u0E1E\u0E18\u0E4C."
lastmod: '2024-04-05T21:54:02.289576-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML"
weight: 40
---

## วิธีการ:
```PowerShell
# โหลดไฟล์ XML เข้าไปในตัวแปร
[xml]$xmlContent = Get-Content 'path\to\your\file.xml'

# เข้าถึงโหนด XML
$books = $xmlContent.catalog.book
foreach ($book in $books) {
  Write-Output "ชื่อหนังสือ: $($book.title)"
}

# สร้างองค์ประกอบ XML ใหม่
$newBook = $xmlContent.CreateElement("book")
$newBook.SetAttribute("id", "bk999")
$xmlContent.DocumentElement.AppendChild($newBook)

# บันทึก XML กลับเข้าไฟล์
$xmlContent.Save('path\to\your\updated\file.xml')
```
ตัวอย่างผลลัพธ์:
```
ชื่อหนังสือ: Programming PowerShell
ชื่อหนังสือ: XML Essentials
```

## ศึกษาเพิ่มเติม
XML หรือ eXtensible Markup Language ได้เกิดขึ้นตั้งแต่ปลายยุค '90 และยังคงเป็นรูปแบบที่ใช้กันอย่างแพร่หลายสำหรับข้อมูลที่มีโครงสร้าง PowerShell ทำให้การทำงานกับ XML ง่ายขึ้นเมื่อเทียบกับวิธีการแยกส่วนทางดั้งเดิม; มันแปลง XML เป็นอ็อบเจ็กต์โดยตรง, ทำให้คุณสามารถโต้ตอบกับองค์ประกอบโดยใช้การเข้าถึงแบบจุดที่คุ้นเคย

ทางเลือกอื่นสำหรับ XML รวมถึง JSON, YAML, หรือรูปแบบข้อมูลที่กำหนดเอง ตัวอย่างเช่น JSON ได้รับความนิยมสำหรับความเบาบางและความง่ายในการใช้งานกับเทคโนโลยีเว็บ อย่างไรก็ตาม, คุณสมบัติขยายเช่นเนมสเปซ, โครงสร้างและการประมวลผล XSLT ของ XML มักทำให้มันเหมาะสมกว่าสำหรับเอกสารที่ซับซ้อนหรือมาตรฐานอุตสาหกรรม

PowerShell ใช้ความสามารถของ XML ของ .NET Framework ในการจัดการ XML นี้หมายความว่ามันไม่เพียงแค่เกี่ยวกับการดำเนินการอ่าน-เขียนง่ายๆ; คุณยังสามารถทำงานกับโครงสร้าง XML สำหรับการตรวจสอบความถูกต้อง, ใช้ XPath สำหรับการคิวรี, และใช้การแปลง XSLT, ทั้งหมดผ่าน PowerShell

## ดูเพิ่มเติม
- [W3Schools บทเรียน XML](https://www.w3schools.com/xml/)
- [XML กับ JSON](https://www.json.org/json-en.html)
