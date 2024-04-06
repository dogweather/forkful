---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:04.829556-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 VBA \u0E2A\u0E32\
  \u0E21\u0E32\u0E23\u0E16\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\u0E17\
  \u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E44\u0E14\u0E49\
  \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\
  \ `Replace` \u0E2B\u0E23\u0E37\u0E2D\u0E1C\u0E48\u0E32\u0E19\u0E42\u0E21\u0E40\u0E14\
  \u0E25\u0E27\u0E31\u0E15\u0E16\u0E38\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E43\u0E19\u0E42\
  \u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E1B\u0E23\u0E30\u0E22\u0E38\u0E01\u0E15\u0E4C\
  \u0E40\u0E0A\u0E48\u0E19 Excel \u0E2B\u0E23\u0E37\u0E2D Word\u2026"
lastmod: '2024-03-17T21:57:56.022262-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 VBA \u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E04\u0E49\u0E19\u0E2B\
  \u0E32\u0E41\u0E25\u0E30\u0E41\u0E17\u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\
  \u0E27\u0E32\u0E21\u0E44\u0E14\u0E49\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E1F\u0E31\
  \u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19 `Replace` \u0E2B\u0E23\u0E37\u0E2D\u0E1C\u0E48\
  \u0E32\u0E19\u0E42\u0E21\u0E40\u0E14\u0E25\u0E27\u0E31\u0E15\u0E16\u0E38\u0E40\u0E09\
  \u0E1E\u0E32\u0E30\u0E43\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E1B\u0E23\
  \u0E30\u0E22\u0E38\u0E01\u0E15\u0E4C\u0E40\u0E0A\u0E48\u0E19 Excel \u0E2B\u0E23\u0E37\
  \u0E2D Word \u0E14\u0E49\u0E32\u0E19\u0E25\u0E48\u0E32\u0E07\u0E40\u0E1B\u0E47\u0E19\
  \u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E17\u0E35\u0E48\u0E41\u0E2A\u0E14\
  \u0E07\u0E17\u0E31\u0E49\u0E07\u0E2A\u0E2D\u0E07\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\
  \u0E23\n"
title: "\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E41\u0E17\
  \u0E19\u0E17\u0E35\u0E48\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 10
---

## วิธีการ:
ใน VBA สามารถค้นหาและแทนที่ข้อความได้โดยใช้ฟังก์ชัน `Replace` หรือผ่านโมเดลวัตถุเฉพาะในโปรแกรมประยุกต์เช่น Excel หรือ Word ด้านล่างเป็นตัวอย่างที่แสดงทั้งสองวิธีการ

### การใช้ฟังก์ชัน `Replace`:
ฟังก์ชัน `Replace` เป็นวิธีที่ตรงไปตรงมาสำหรับการแทนที่ข้อความง่ายๆ มีรูปแบบ `Replace(expression, find, replaceWith[, start[, count[, compare]]])`.

ตัวอย่าง:
```vb
Dim originalText As String
Dim newText As String

originalText = "Hello, World! Programming in VBA is fun."
newText = Replace(originalText, "World", "Everyone")

Debug.Print newText
```
ผลลัพธ์:
```
Hello, Everyone! Programming in VBA is fun.
```

### การค้นหาและแทนที่ใน Excel:
สำหรับ Excel คุณสามารถใช้เมธอด `Range.Replace` ซึ่งให้การควบคุมที่มากขึ้น เช่น ความไวต่อตัวพิมพ์เล็กพิมพ์ใหญ่และการแทนที่คำทั้งหมด

ตัวอย่าง:
```vb
Sub ReplaceTextInExcel()
    Dim ws As Worksheet
    Set ws = ThisWorkbook.Sheets("Sheet1")

    With ws.Range("A1:A100") ' กำหนดช่วงที่คุณต้องการค้นหา
        .Replace What:="old", Replacement:="new", MatchCase:=False, LookAt:=xlPart
    End With
End Sub
```

### การค้นหาและแทนที่ใน Word:
อย่างเช่นเดียวกัน Word มีคุณสมบัติ `Find` และ `Replace` ที่ทรงพลัง ซึ่งสามารถเข้าถึงได้ผ่าน VBA

ตัวอย่าง:
```vb
Sub ReplaceTextInWord()
    Dim doc As Document
    Set doc = ActiveDocument
    
    With doc.Content.Find
        .Text = "specific"
        .Replacement.Text = "particular"
        .Execute Replace:=wdReplaceAll
    End With
End Sub
```

## ดูข้อมูลเพิ่มเติม:
การค้นหาและแทนที่ข้อความใน VBA มีความสำคัญต่อความสามารถในการอัตโนมัติของแอปพลิเคชัน Microsoft Office อย่างเป็นรูปธรรม ช่วยเพิ่มผลผลิตโดยการเขียนสคริปต์สำหรับงานที่ซ้ำซาก ตลอดเวลา ฟังก์ชันเหล่านี้ได้พัฒนามากขึ้นจนเป็นที่มีพลังและความยืดหยุ่นมากขึ้น ตอบโจทย์ในหลากหลายกรณีการใช้งาน

ทั้งนี้ แม้ว่าฟังก์ชัน `Replace` ของ VBA จะเหมาะสำหรับการดำเนินการข้อความง่ายๆ โมเดลวัตถุของ Excel และ Word ให้การควบคุมที่มากกว่าและควรจะใช้สำหรับงานที่เฉพาะเจาะจงของแอปพลิเคชัน พวกเขาสนับสนุนคุณสมบัติขั้นสูงเช่นการจับคู่รูปแบบ การรักษาการจัดรูปแบบ และเกณฑ์การค้นหาที่ละเอียดอ่อน (เช่น การตรงกับกรณี คำทั้งหมด)

อย่างไรก็ตาม VBA และความสามารถในการจัดการข้อความของมัน แม้ว่าจะเข้มแข็งภายในระบบนิเวศของ Microsoft อาจไม่เสมอไปเป็นเครื่องมือที่ดีที่สุดสำหรับการประมวลผลข้อความที่มีประสิทธิภาพสูงหรือต้องการความซับซ้อนมากขึ้น ภาษาเช่น Python พร้อมกับไลบรารีเช่น `re` สำหรับการแสดงออกปกติ ให้ตัวเลือกในการจัดการข้อความที่มีพลังและหลากหลายมากขึ้น แต่สำหรับผู้ที่ทำงานภายในแอปพลิเคชัน Microsoft Office VBA ยังคงเป็นตัวเลือกที่เข้าถึงได้และมีประสิทธิภาพสำหรับงานอัตโนมัติในการค้นหาและแทนที่
