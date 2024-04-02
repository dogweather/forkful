---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:45.353790-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E14\
  \u0E49\u0E27\u0E22\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E2A\
  \u0E34\u0E17\u0E18\u0E34\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19 (Basic Authentication)\
  \ \u0E43\u0E19 Visual Basic for Applications (VBA)\u2026"
lastmod: '2024-03-17T21:57:56.037707-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E14\
  \u0E49\u0E27\u0E22\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E2A\
  \u0E34\u0E17\u0E18\u0E34\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19 (Basic Authentication)\
  \ \u0E43\u0E19 Visual Basic for Applications (VBA)\u2026"
title: "\u0E01\u0E32\u0E23\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP \u0E14\u0E49\
  \u0E27\u0E22\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E2A\u0E34\
  \u0E17\u0E18\u0E34\u0E4C\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19"
weight: 45
---

## อะไร & ทำไม?

การส่งคำขอ HTTP ด้วยการตรวจสอบสิทธิพื้นฐาน (Basic Authentication) ใน Visual Basic for Applications (VBA) เป็นการเข้าถึงทรัพยากรเว็บที่ได้รับการป้องกันด้วยชื่อผู้ใช้และรหัสผ่าน โปรแกรมเมอร์ทำเช่นนี้เพื่อโต้ตอบกับ APIs หรือเว็บเซอร์วิสที่มีความปลอดภัยภายในแอปพลิเคชันที่ขับเคลื่อนด้วย VBA ตัวอย่างเช่น การทำงานอัตโนมัติใน Excel หรือ Access ด้วยข้อมูลจากจุดสิ้นสุดที่ได้รับการป้องกัน

## วิธีการ:

ใน VBA คุณสามารถใช้ห้องสมุด `Microsoft XML, v6.0` (MSXML2) เพื่อส่งคำขอ HTTP ด้วยการตรวจสอบสิทธิพื้นฐาน วิธีนี้รวมถึงการตั้งค่าหัวข้อ `"Authorization"` ของคำขอเพื่อรวมข้อมูลประจำตัวในรูปแบบที่เข้ารหัส base64 นี่คือคู่มือทีละขั้นตอน:

1. **อ้างอิง MSXML2**: ขั้นแรก ให้แน่ใจว่าโปรเจ็กต์ VBA ของคุณอ้างอิงไลบรารี `Microsoft XML, v6.0` ในตัวแก้ไข VBA ไปที่ Tools > References และตรวจสอบ `Microsoft XML, v6.0`.

2. **สร้างและส่งคำขอ HTTP**: ใช้ตัวอย่างโค้ด VBA ต่อไปนี้เป็นคู่มือ แทนที่ `"your_username"` และ `"your_password"` ด้วยข้อมูลประจำตัวจริงของคุณและปรับ URL ตามที่ต้องการ

    ```vb
    Dim XMLHttp As Object
    Set XMLHttp = CreateObject("MSXML2.XMLHTTP")
    Dim url As String
    url = "http://example.com/api/resource" ' แทนที่ด้วย URL จริง
    Dim base64Credentials As String
    base64Credentials = EncodeBase64("your_username:your_password")
    
    XMLHttp.Open "GET", url, False
    XMLHttp.setRequestHeader "Authorization", "Basic " & base64Credentials
    XMLHttp.send
    
    Debug.Print XMLHttp.responseText ' แสดงผลการตอบกลับไปยังหน้าต่าง Immediate
    ```

3. **เข้ารหัสข้อมูลประจำตัวในรูปแบบ base64**: VBA ไม่มีฟังก์ชันในตัวสำหรับการเข้ารหัส base64 แต่คุณสามารถใช้ฟังก์ชัน `EncodeBase64` ที่กำหนดเองนี้:

    ```vb
    Function EncodeBase64(text As String) As String
        Dim arrData() As Byte
        arrData = StrConv(text, vbFromUnicode)
        
        Dim objXML As MSXML2.DOMDocument60
        Dim objNode As MSXML2.IXMLDOMElement
        
        Set objXML = New MSXML2.DOMDocument60
        Set objNode = objXML.createElement("b64")
        
        objNode.dataType = "bin.base64"
        objNode.nodeTypedValue = arrData
        EncodeBase64 = objNode.Text
    End Function
    ```
    
สิ่งนี้จะส่งคำขอ GET ไปที่ `http://example.com/api/resource` พร้อมข้อมูลประจำตัวการตรวจสอบสิทธิพื้นฐานที่ระบุ และพิมพ์คำตอบ

## ศึกษาเพิ่มเติม

วิธีการที่ใช้ที่นี่ แม้ว่าจะมีประสิทธิผลสำหรับกรณีการใช้งานง่ายๆ ก็ตาม แต่ขึ้นอยู่กับโครงสร้าง Basic Authentication ซึ่งส่งข้อมูลประจำตัวในรูปแบบที่สามารถถอดรหัสได้ง่าย (การเข้ารหัส base64 ไม่ใช่การเข้ารหัส) เนื่องจากมีความเสี่ยง โดยเฉพาะในบริบทที่ไม่ใช่ HTTPS Basic Authentication จึงไม่แนะนำสำหรับการส่งข้อมูลที่ละเอียดอ่อนผ่านอินเทอร์เน็ตโดยไม่มีชั้นความปลอดภัยเพิ่มเติมเช่น SSL/TLS

โดยประวัติศาสตร์แล้ว Basic Authentication เป็นหนึ่งในวิธีแรกๆ ที่พัฒนาขึ้นเพื่อควบคุมการเข้าถึงทรัพยากรเว็บ ในปัจจุบัน มาตรฐานการตรวจสอบสิทธิ์ที่ปลอดภัยและยืดหยุ่นกว่า เช่น OAuth 2.0 มักถูกใช้สำหรับแอปพลิเคชันใหม่ๆ โดยทั่วไป เนื่องจากข้อจำกัดและความพึ่งพาภายนอกสำหรับวิธีการตรวจสอบสิทธิ์ที่ยากขึ้น นักพัฒนามักใช้ VBA ในสภาพแวดล้อมภายในหรือที่มีความสำคัญด้านความปลอดภัยน้อยลง หรือใช้เป็นก้าวขึ้นสู่การสร้างต้นแบบความคิดอย่างรวดเร็ว

เมื่อใช้ VBA สำหรับคำขอ HTTP จำไว้ว่าแต่ละเวอร์ชันของห้องสมุด MSXML อาจรองรับคุณสมบัติและมาตรฐานความปลอดภัยที่แตกต่างกัน ใช้เวอร์ชันล่าสุดที่เข้ากันได้กับแอปพลิเคชันของคุณเพื่อความปลอดภัยและประสิทธิภาพที่ดีขึ้น นอกจากนี้ พิจารณาข้อจำกัดด้านสิ่งแวดล้อมและคุณสมบัติที่อาจล้าสมัยเมื่อเลือกใช้ VBA สำหรับโปรเจ็กต์ใหม่ๆ โดยเฉพาะโครงการที่ต้องการการสื่อสาร HTTP ที่ปลอดภัย สภาพแวดล้อมการเขียนโปรแกรมหรือภาษาอื่นๆ อาจเสนอโซลูชันที่มั่นคง ปลอดภัย และสามารถบำรุงรักษาได้มากกว่าสำหรับงานที่คล้ายคลึงกัน
