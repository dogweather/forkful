---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:18.487017-06:00
description: "JSON (JavaScript Object Notation) \u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\
  \u0E1B\u0E41\u0E1A\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E25\u0E01\u0E40\u0E1B\u0E25\u0E35\
  \u0E48\u0E22\u0E19\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E40\u0E1A\
  \u0E32\u0E41\u0E25\u0E30\u0E07\u0E48\u0E32\u0E22\u0E15\u0E48\u0E2D\u0E01\u0E32\u0E23\
  \u0E2D\u0E48\u0E32\u0E19\u0E41\u0E25\u0E30\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E14\
  \u0E22\u0E21\u0E19\u0E38\u0E29\u0E22\u0E4C\u2026"
lastmod: '2024-03-17T21:57:56.059963-06:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) \u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\
  \u0E41\u0E1A\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E25\u0E01\u0E40\u0E1B\u0E25\u0E35\u0E48\
  \u0E22\u0E19\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E40\u0E1A\u0E32\
  \u0E41\u0E25\u0E30\u0E07\u0E48\u0E32\u0E22\u0E15\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E2D\
  \u0E48\u0E32\u0E19\u0E41\u0E25\u0E30\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E14\u0E22\
  \u0E21\u0E19\u0E38\u0E29\u0E22\u0E4C\u2026"
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A JSON"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

JSON (JavaScript Object Notation) เป็นรูปแบบการแลกเปลี่ยนข้อมูลที่เบาและง่ายต่อการอ่านและเขียนโดยมนุษย์ และสำหรับเครื่องจักรในการแยกวิเคราะห์และสร้างข้อมูล โปรแกรมเมอร์ใช้ JSON เพื่อส่งข้อมูลระหว่างเซิร์ฟเวอร์และแอปพลิเคชันเว็บ หรือเพื่อเก็บข้อมูลในรูปแบบที่มีโครงสร้างและสามารถเข้าถึงได้ง่ายในสภาพแวดล้อมการเขียนโปรแกรมที่หลากหลาย รวมถึง Visual Basic for Applications (VBA)

## วิธีการ:

VBA ไม่รองรับการแยกวิเคราะห์หรือการสร้าง JSON แบบเนทีฟ ดังนั้นเราจะใช้ภาษาสคริปต์เช่น JScript (ผ่านอ็อบเจกต์ ScriptControl) สำหรับการวิเคราะห์สตริง JSON และการสร้างอ็อบเจกต์ JSON นี่คือวิธีที่คุณสามารถแยกวิเคราะห์สตริง JSON ใน VBA:

```basic
Function ParseJSON(ByVal jsonString As String) As Object
    Dim scriptControl As Object
    Set scriptControl = CreateObject("MSScriptControl.ScriptControl")
    scriptControl.Language = "JScript"
    
    scriptControl.Eval "var obj = (" & jsonString & ")"
    Set ParseJSON = scriptControl.CodeObject.obj
End Function

Sub DemoParseJSON()
    Dim jsonString As String
    jsonString = "{""name"":""John"", ""age"":30, ""city"":""New York""}"
    
    Dim parsed As Object
    Set parsed = ParseJSON(jsonString)
    
    MsgBox "Name: " & parsed.name & ", Age: " & parsed.age & ", City: " & parsed.city
End Sub
```

เพื่อสร้าง JSON, คุณสามารถใช้วิธีที่คล้ายกันได้ โดยการสร้างสตริง JSON ผ่านการเชื่อมต่อ:

```basic
Function GenerateJSON(name As String, age As Integer, city As String) As String
    GenerateJSON = "{""name"":""" & name & """, ""age"":" & age & ", ""city"":""" & city & """}"
End Function

Sub DemoGenerateJSON()
    Dim jsonString As String
    jsonString = GenerateJSON("Jane", 28, "Los Angeles")
    
    MsgBox jsonString
End Sub
```

## การศึกษาลึก

วิธีการที่แสดงไว้ใช้ประโยชน์จาก ScriptControl เพื่อจัดการกับ JSON โดยพื้นฐานแล้วคือการมอบงานให้กับเครื่องยนต์ JavaScript นี่เป็นวิธีการแก้ปัญหาทางอ้อมที่สร้างสรรค์ แต่ไม่จำเป็นต้องเป็นวิธีที่มีประสิทธิภาพหรือทันสมัยที่สุดในการทำงานกับ JSON ในบริบทของ VBA ในการใช้งานที่ซับซ้อนมากขึ้น วิธีนี้อาจกลายเป็นเรื่องยุ่งยากและนำมาซึ่งภาระด้านประสิทธิภาพหรือปัญหาด้านความปลอดภัย เนื่องจาก ScriptControl ทำงานในสภาพแวดล้อมที่มีการเข้าถึงคอมพิวเตอร์โฮสต์เต็มรูปแบบ

สภาพแวดล้อมการเขียนโปรแกรมอื่น ๆ เช่น Python หรือ JavaScript มีการรองรับ JSON ในตัว ทำให้เหมาะสมกว่าสำหรับแอปพลิเคชันที่ต้องการการจัดการ JSON อย่างกว้างขวาง ภาษาเหล่านี้มีไลบรารีอย่างครอบคลุมที่ช่วยให้สามารถทำการวิเคราะห์และสร้าง รวมถึงการค้นหาและการจัดรูปแบบข้อมูล JSON

แม้จะมีข้อจำกัดเหล่านี้ใน VBA แต่การเข้าใจวิธีการทำงานกับ JSON เป็นสิ่งสำคัญในโลกที่การแลกเปลี่ยนข้อมูลบนเว็บและไฟล์การกำหนดค่ามักจะถูกจัดรูปแบบเป็น JSON สำหรับโปรแกรมเมอร์ VBA การเรียนรู้เทคนิคเหล่านี้สร้างโอกาสในการรวมกับเว็บ API การตีความไฟล์การกำหนดค่า หรือกระทั่งการสร้างเว็บแอปพลิเคชันง่ายๆ อย่างไรก็ตาม เมื่อโปรเจกต์เติบโตขึ้นเรื่อยๆ หรือต้องการประสิทธิภาพสูง นักพัฒนาอาจพิจารณาใช้สภาพแวดล้อมการเขียนโปรแกรมที่เอื้อต่อการจัดการกับ JSON มากยิ่งขึ้น
