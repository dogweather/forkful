---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:49.678225-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E01\u0E32\u0E23\u0E17\u0E33\
  \u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML, \u0E42\u0E14\u0E22\u0E1B\u0E01\u0E15\u0E34\
  \u0E41\u0E25\u0E49\u0E27\u0E08\u0E30\u0E43\u0E0A\u0E49 `MSXML2.DOMDocument` \u0E27\
  \u0E31\u0E15\u0E16\u0E38 \u0E2D\u0E34\u0E19\u0E40\u0E17\u0E2D\u0E23\u0E4C\u0E40\u0E1F\
  \u0E2A\u0E19\u0E35\u0E49\u0E0A\u0E48\u0E27\u0E22\u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\
  \u0E42\u0E2B\u0E25\u0E14, \u0E41\u0E22\u0E01\u0E2A\u0E48\u0E27\u0E19, \u0E41\u0E25\
  \u0E30\u0E19\u0E33\u0E17\u0E32\u0E07\u0E40\u0E2D\u0E01\u0E2A\u0E32\u0E23 XML\u2026"
lastmod: '2024-03-17T21:57:56.062781-06:00'
model: gpt-4-0125-preview
summary: "\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\
  \u0E19\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML, \u0E42\
  \u0E14\u0E22\u0E1B\u0E01\u0E15\u0E34\u0E41\u0E25\u0E49\u0E27\u0E08\u0E30\u0E43\u0E0A\
  \u0E49 `MSXML2.DOMDocument` \u0E27\u0E31\u0E15\u0E16\u0E38 \u0E2D\u0E34\u0E19\u0E40\
  \u0E17\u0E2D\u0E23\u0E4C\u0E40\u0E1F\u0E2A\u0E19\u0E35\u0E49\u0E0A\u0E48\u0E27\u0E22\
  \u0E43\u0E2B\u0E49\u0E04\u0E38\u0E13\u0E42\u0E2B\u0E25\u0E14, \u0E41\u0E22\u0E01\
  \u0E2A\u0E48\u0E27\u0E19, \u0E41\u0E25\u0E30\u0E19\u0E33\u0E17\u0E32\u0E07\u0E40\
  \u0E2D\u0E01\u0E2A\u0E32\u0E23 XML \u0E14\u0E49\u0E32\u0E19\u0E25\u0E48\u0E32\u0E07\
  \u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E07\u0E48\
  \u0E32\u0E22\u0E46 \u0E17\u0E35\u0E48\u0E41\u0E2A\u0E14\u0E07\u0E27\u0E34\u0E18\u0E35\
  \u0E01\u0E32\u0E23\u0E42\u0E2B\u0E25\u0E14\u0E44\u0E1F\u0E25\u0E4C XML, \u0E19\u0E33\
  \u0E17\u0E32\u0E07\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07 \u0E41\u0E25\
  \u0E30\u0E2D\u0E48\u0E32\u0E19\u0E04\u0E38\u0E13\u0E2A\u0E21\u0E1A\u0E31\u0E15\u0E34\
  \u0E41\u0E25\u0E30\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E02\u0E49\u0E2D\u0E04\
  \u0E27\u0E32\u0E21."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A XML"
weight: 40
---

## วิธีการ:
เพื่อเริ่มต้นการทำงานกับ XML, โดยปกติแล้วจะใช้ `MSXML2.DOMDocument` วัตถุ อินเทอร์เฟสนี้ช่วยให้คุณโหลด, แยกส่วน, และนำทางเอกสาร XML ด้านล่างเป็นตัวอย่างง่ายๆ ที่แสดงวิธีการโหลดไฟล์ XML, นำทางโครงสร้าง และอ่านคุณสมบัติและเนื้อหาข้อความ

```basic
' ขั้นแรก, ตรวจสอบว่าคุณได้เพิ่มอ้างอิงไปยัง "Microsoft XML, v6.0" ผ่าน Tools -> References
Dim xmlDoc As MSXML2.DOMDocument60
Set xmlDoc = New MSXML2.DOMDocument60
xmlDoc.async = False
xmlDoc.Load("C:\Path\To\Your\File.xml") ' โหลดไฟล์ XML ของคุณ

' ตรวจสอบว่า XML ถูกโหลดสำเร็จหรือไม่
If xmlDoc.parseError.ErrorCode <> 0 Then
    MsgBox "Error loading XML:" & xmlDoc.parseError.reason
Else
    ' นำทางและอ่านองค์ประกอบ
    Dim book As IXMLDOMNode
    Set book = xmlDoc.SelectSingleNode("//book/title") ' XPath เพื่อค้นหา <title> แรกภายใน <book>
    MsgBox book.Text ' แสดงข้อความของ title
End If
```

ในตัวอย่างข้างต้น, เราสร้างอินสแตนซ์ของ `MSXML2.DOMDocument60`, โหลดไฟล์ XML, และจากนั้นตรวจสอบสำหรับข้อผิดพลาด หากไม่พบข้อผิดพลาด เราจะนำทางไปยังโหนดที่ระบุโดยใช้ XPath และแสดงเนื้อหาข้อความของมัน

## ลงลึก:
การบูรณาการความสามารถของ XML ใน VBA เริ่มต้นขึ้นตั้งแต่ต้นยุค 2000 เมื่อความต้องการให้แอพพลิเคชัน Office ทำงานร่วมกับข้อมูลเว็บและบริการเริ่มเติบโตขึ้น ไลบรารี `MSXML`, หรือ Microsoft XML Core Services, ได้พัฒนาขึ้นเรื่อยๆ โดย `MSXML2.DOMDocument60` เป็นหนึ่งในเวอร์ชันล่าสุดที่แนะนำให้ใช้เนื่องจากมีประสิทธิภาพและความปลอดภัยที่เพิ่มขึ้น

แม้ว่าจะมีประสิทธิภาพ, ความสามารถในการจัดการ XML ของ VBA ถือว่าไม่มีประสิทธิภาพและซับซ้อนเมื่อเปรียบเทียบกับสภาพแวดล้อมการเขียนโปรแกรมสมัยใหม่ เช่น XML.etree ของ Python หรือ LINQ to XML ของ C# ความยืดยาวของ VBA และความต้องการในการเพิ่มและจัดการอ้างอิงด้วยตนเองอาจเป็นอุปสรรคต่อการพัฒนาอย่างรวดเร็ว นอกจากนี้, ด้วยการมาถึงของ JSON ในฐานะรูปแบบการแลกเปลี่ยนข้อมูลที่เบากว่า หลายโปรแกรมเมอร์และแอพพลิเคชันกำลังเปลี่ยนไปใช้ JSON แทน XML เว้นแต่การทำงานร่วมกับระบบเก่าหรือบริการองค์กรเฉพาะที่จำเป็นต้องใช้ XML

อย่างไรก็ตาม, สำหรับงานที่ต้องการการแยกส่วนหรือสร้างเอกสาร XML ภายในบริบทของการอัตโนมัติของ Microsoft Office, การใช้คุณสมบัติการจัดการ XML ของ VBA ยังคงเป็นวิธีการที่ทำได้และบางครั้งจำเป็น สิ่งนี้สร้างความสมดุลระหว่างการเข้าถึงชุดคุณสมบัติที่หลากหลายของแอพพลิเคชัน Office และความสามารถในการจัดการข้อมูลที่มีโครงสร้างที่ XML มอบให้
