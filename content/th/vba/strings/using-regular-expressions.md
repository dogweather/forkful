---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:26.325249-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E2B\u0E32\u0E01\u0E15\
  \u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 regular expressions \u0E43\
  \u0E19 VBA \u0E42\u0E14\u0E22\u0E41\u0E23\u0E01\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\
  \u0E49\u0E2D\u0E07\u0E40\u0E1B\u0E34\u0E14\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19 Microsoft\
  \ VBScript Regular Expressions library \u0E43\u0E19 VBA editor, \u0E44\u0E1B\u0E17\
  \u0E35\u0E48 `Tools` ->\u2026"
lastmod: '2024-03-17T21:57:56.027546-06:00'
model: gpt-4-0125-preview
summary: "\u0E2B\u0E32\u0E01\u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E43\u0E0A\
  \u0E49 regular expressions \u0E43\u0E19 VBA \u0E42\u0E14\u0E22\u0E41\u0E23\u0E01\
  \u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E2D\u0E07\u0E40\u0E1B\u0E34\u0E14\u0E43\
  \u0E0A\u0E49\u0E07\u0E32\u0E19 Microsoft VBScript Regular Expressions library \u0E43\
  \u0E19 VBA editor, \u0E44\u0E1B\u0E17\u0E35\u0E48 `Tools` -> `References`, \u0E08\
  \u0E32\u0E01\u0E19\u0E31\u0E49\u0E19\u0E40\u0E25\u0E37\u0E2D\u0E01 `Microsoft VBScript\
  \ Regular Expressions 5.5`\n\n\u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\
  \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E43\u0E19\
  \u0E01\u0E32\u0E23\u0E2B\u0E32\u0E27\u0E48\u0E32\u0E21\u0E35\u0E23\u0E39\u0E1B\u0E41\
  \u0E1A\u0E1A\u0E17\u0E35\u0E48\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E2D\u0E22\u0E39\u0E48\
  \u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E21\u0E48\
  ."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E40\u0E23\u0E01\u0E38\u0E25\u0E32\u0E23\
  \u0E4C\u0E40\u0E2D\u0E47\u0E01\u0E40\u0E1E\u0E23\u0E2A\u0E0A\u0E31\u0E19"
weight: 11
---

## วิธีการ:
หากต้องการใช้ regular expressions ใน VBA โดยแรกเริ่มต้องเปิดใช้งาน Microsoft VBScript Regular Expressions library ใน VBA editor, ไปที่ `Tools` -> `References`, จากนั้นเลือก `Microsoft VBScript Regular Expressions 5.5`

นี่คือตัวอย่างพื้นฐานในการหาว่ามีรูปแบบที่กำหนดอยู่ในสตริงหรือไม่:

```vb
Sub FindPattern()
    Dim regex As Object
    Set regex = CreateObject("VBScript.RegExp")

    With regex
        .Global = True
        .IgnoreCase = True
        .Pattern = "\bis\b"  ' ค้นหาคำ "is"
    End With
    
    Dim testString As String
    testString = "This is a test string."
    
    If regex.Test(testString) Then
        MsgBox "พบรูปแบบ."
    Else
        MsgBox "ไม่พบรูปแบบ."
    End If
End Sub
```

ในการแทนที่รูปแบบในสตริง:

```vb
Sub ReplacePattern()
    Dim regex As Object, replacedString As String
    Set regex = CreateObject("VBScript.RegExp")
    
    With regex
        .Global = True
        .IgnoreCase = False
        .Pattern = "\s"  ' จับคู่กับอักขระว่างใด ๆ
    End With
    
    replacedString = regex.Replace("This is a test string.", "_")
    MsgBox replacedString  ' ผลลัพธ์: "This_is_a_test_string."
End Sub
```

## ศึกษาเพิ่มเติม
การรวม regular expressions เข้ากับภาษาโปรแกรมมิ่งมักจะสืบเนื่องมาจากเครื่องมือ Unix ในยุค 1970  VBA ได้รวม regex ผ่าน VBScript Regular Expressions library, ซึ่งเน้นถึงความสำคัญในงานประมวลผลข้อความ แม้ในแอปพลิเคชันที่ไม่มีความเกี่ยวข้องโดยตรงกับการจัดการข้อความจำนวนมากเช่น Excel หรือ Access

แม้จะมีพลัง, regex ใน VBA บางครั้งอาจไม่ง่ายต่อการเรียนรู้หรือประสิทธิภาพไม่เท่ากับการใช้งานในภาษาที่ทันสมัย อย่างเช่น Python หรือ JavaScript เป็นต้น Python ด้วยโมดูล `re` นั้นให้การสนับสนุนอย่างกว้างขวางสำหรับกลุ่มที่มีชื่อและความสามารถในการจับคู่รูปแบบที่ซับซ้อนยิ่งขึ้น ให้วิธีที่สะอาดและอาจอ่านได้ง่ายกว่าแม้ว่าเมื่อทำงานภายในระบบนิเวศ VBA, regular expressions ยังคงเป็นเครื่องมือที่มีคุณค่าสำหรับงานที่ต้องการการจับคู่รูปแบบหรือการจัดการข้อความ การแลกเปลี่ยนประสิทธิภาพนั้นมักจะเป็นเรื่องที่สามารถละเลยได้เมื่อพิจารณาถึงความสะดวกและความสามารถที่ regex มอบเมื่อจัดการกับสตริงในแอปพลิเคชัน Office
