---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:25.824722-06:00
description: "\u0E01\u0E32\u0E23 Refactor \u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\u0E02\
  \u0E35\u0E22\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E2B\u0E21\u0E32\u0E22\
  \u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E41\u0E01\u0E49\u0E44\u0E02\u0E42\u0E04\u0E23\
  \u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E2D\u0E07\u0E42\u0E04\u0E49\u0E14\u0E42\
  \u0E14\u0E22\u0E44\u0E21\u0E48\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E41\u0E1B\
  \u0E25\u0E07\u0E1E\u0E24\u0E15\u0E34\u0E01\u0E23\u0E23\u0E21\u0E02\u0E2D\u0E07\u0E21\
  \u0E31\u0E19 \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E1B\u0E23\u0E31\u0E1A\u0E1B\u0E23\u0E38\
  \u0E07\u0E40\u0E0A\u0E48\u0E19\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E32\u0E21\u0E32\u0E23\
  \u0E16\u0E43\u0E19\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19, \u0E01\u0E32\u0E23\
  \u0E1A\u0E33\u0E23\u0E38\u0E07\u0E23\u0E31\u0E01\u0E29\u0E32,\u2026"
lastmod: '2024-03-17T21:57:56.047178-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23 Refactor \u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\
  \u0E22\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E2B\u0E21\u0E32\u0E22\u0E16\
  \u0E36\u0E07\u0E01\u0E32\u0E23\u0E41\u0E01\u0E49\u0E44\u0E02\u0E42\u0E04\u0E23\u0E07\
  \u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E2D\u0E07\u0E42\u0E04\u0E49\u0E14\u0E42\u0E14\
  \u0E22\u0E44\u0E21\u0E48\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E41\u0E1B\u0E25\
  \u0E07\u0E1E\u0E24\u0E15\u0E34\u0E01\u0E23\u0E23\u0E21\u0E02\u0E2D\u0E07\u0E21\u0E31\
  \u0E19 \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E1B\u0E23\u0E31\u0E1A\u0E1B\u0E23\u0E38\u0E07\
  \u0E40\u0E0A\u0E48\u0E19\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\
  \u0E43\u0E19\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19, \u0E01\u0E32\u0E23\u0E1A\
  \u0E33\u0E23\u0E38\u0E07\u0E23\u0E31\u0E01\u0E29\u0E32,\u2026"
title: "\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E42\u0E04\u0E49\u0E14"
weight: 19
---

## อะไร & ทำไม?

การ Refactor ในการเขียนโปรแกรมหมายถึงการแก้ไขโครงสร้างของโค้ดโดยไม่เปลี่ยนแปลงพฤติกรรมของมัน เพื่อปรับปรุงเช่นความสามารถในการอ่าน, การบำรุงรักษา, หรือประสิทธิภาพ โปรแกรมเมอร์ต้องการ refactor เพื่อให้โค้ดมีประสิทธิภาพมากขึ้น, ง่ายต่อการเข้าใจ, ง่ายต่อการแก้ไขในอนาคต, และลดความน่าจะเป็นของบั๊ก

## วิธีการ:

พิจารณาตัวอย่างพื้นฐานใน Visual Basic for Applications (VBA) ที่เรามี subroutine ซึ่งพิมพ์รายละเอียดของพนักงาน ในตอนแรกโค้ดนั้นยุ่ง ยากต่อการดูแลหรือขยาย

```vb
Sub PrintEmployeeDetails()
    Dim name As String
    Dim age As Integer
    Dim department As String
    name = "John Doe"
    age = 30
    department = "IT"
    
    MsgBox "Name: " & name & vbCrLf & "Age: " & age & vbCrLf & "Department: " & department
End Sub
```

ขั้นตอนการ Refactor ที่ 1: สร้าง Method แยกต่างหาก หนึ่งในเทคนิคการ refactor ที่พบบ่อยที่สุดคือการเอาชิ้นของโค้ดเฉพาะออกไปไว้ใน method ของมันเอง จะทำให้โค้ดมีโครงสร้างมากขึ้นและง่ายต่อการเข้าใจ

```vb
Sub PrintEmployeeDetails()
    Dim name As String
    Dim age As Integer
    Dim department As String
    name = "John Doe"
    age = 30
    department = "IT"
    
    DisplayMessage name, age, department
End Sub

Private Sub DisplayMessage(name As String, age As Integer, department As String)
    MsgBox "Name: " & name & vbCrLf & "Age: " & age & vbCrLf & "Department: " & department
End Sub
```

ขั้นตอนการ Refactor ที่ 2: ใช้โครงสร้างข้อมูล เป็นขั้นตอนที่ใช้โครงสร้างข้อมูลในการจัดเก็บข้อมูลที่เกี่ยวข้องกัน เพื่อปรับปรุงความชัดเจนของโค้ดและทำให้สามารถส่งข้อมูลกลุ่มได้ง่ายขึ้น

```vb
Type Employee
    name As String
    age As Integer
    department As String
End Type

Sub PrintEmployeeDetails()
    Dim emp As Employee
    emp.name = "John Doe"
    emp.age = 30
    emp.department = "IT"
    
    DisplayMessage emp
End Sub

Private Sub DisplayMessage(emp As Employee)
    MsgBox "Name: " & emp.name & vbCrLf & "Age: " & emp.age & vbCrLf & "Department: " & emp.department
End Sub
```

ขั้นตอนเหล่านี้เปลี่ยนโค้ดที่ยุ่งเหยิงให้เป็นโค้ดที่มีโครงสร้างและโมดูลาร์ เพิ่มความสามารถในการอ่านและการบำรุงรักษาอย่างมาก

## การศึกษาเพิ่มเติม

แนวคิดเรื่องการ Refactor เก่าพอๆ กับการเขียนโปรแกรมเอง แต่หนังสือ "Refactoring: Improving the Design of Existing Code" ของ Martin Fowler ทำให้มันเข้าสู่กระแสหลัก โดยเน้นความสำคัญในกระบวนการพัฒนาซอฟต์แวร์ ใน Visual Basic for Applications, Refactor อาจเป็นเรื่องที่ท้าทายกว่าเล็กน้อยเนื่องจากขาดเครื่องมือที่ติดตั้งมาใน integrated development environments (IDEs) ที่รองรับการ Refactor อัตโนมัติ

อย่างไรก็ตาม นี่ไม่ได้ลดความสำคัญ แม้ใน VBA, การใช้เทคนิคการ refactor พื้นฐานด้วยตนเองสามารถเพิ่มความสะอาดและประสิทธิภาพของฐานโค้ดได้อย่างมาก แม้ว่า VBA อาจไม่มีความสะดวกสบายร่วมสมัยเหมือนกัน แต่หลักการออกแบบโค้ดที่ดียังคงเป็นสากล เหล่านักพัฒนาที่มาจากภาษาอื่นอาจพบว่ากระบวนการด้วยตนเองนั้นอารมณ์เสีย แต่จะชื่นชมประโยชน์ของการลงทุนเวลาในการปรับปรุงคุณภาพโค้ดตั้งแต่เริ่มต้นอย่างไม่สงสัย

สำหรับสภาพแวดล้อมการพัฒนาที่แข็งแกร่งขึ้นหรือเมื่อทำงานกับโปรเจ็คที่ซับซ้อนมากขึ้น อาจคุ้มค่าที่จะสำรวจทางเลือกอื่นที่เสนอเครื่องมือการ Refactor ที่ทรงพลังขึ้นหรือการเปลี่ยนโครงการ VBA เป็นภาษา .NET ที่ Visual Studio ให้การสนับสนุนการ Refactor อย่างกว้างขวาง อย่างไรก็ตาม การเข้าใจและการใช้หลักการการ Refactor ใน VBA เป็นทักษะที่มีค่าซึ่งเน้นความสำคัญของการเขียนโค้ดที่สะอาดและสามารถบำรุงรักษาได้ ไม่ว่าสภาพแวดล้อมจะเป็นอย่างไร
