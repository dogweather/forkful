---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:35.832579-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 VBA, \u0E01\
  \u0E32\u0E23\u0E43\u0E0A\u0E49\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07 `Debug.Print`\
  \ \u0E04\u0E37\u0E2D\u0E2B\u0E31\u0E27\u0E43\u0E08\u0E2B\u0E25\u0E31\u0E01\u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E02\
  \u0E49\u0E2D\u0E21\u0E39\u0E25\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E44\u0E1B\u0E22\u0E31\
  \u0E07\u0E2B\u0E19\u0E49\u0E32\u0E15\u0E48\u0E32\u0E07 Immediate \u0E43\u0E19 Visual\
  \ Basic Editor (VBE)\u2026"
lastmod: '2024-04-05T21:54:01.610269-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 VBA, \u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E04\u0E33\u0E2A\
  \u0E31\u0E48\u0E07 `Debug.Print` \u0E04\u0E37\u0E2D\u0E2B\u0E31\u0E27\u0E43\u0E08\
  \u0E2B\u0E25\u0E31\u0E01\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E1E\
  \u0E34\u0E21\u0E1E\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E14\u0E35\u0E1A\u0E31\
  \u0E01\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E2B\u0E19\u0E49\u0E32\u0E15\u0E48\u0E32\u0E07\
  \ Immediate \u0E43\u0E19 Visual Basic Editor (VBE) \u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E43\u0E0A\u0E49\u0E1F\u0E35\u0E40\u0E08\u0E2D\u0E23\u0E4C\u0E19\u0E35\u0E49\u0E44\
  \u0E14\u0E49\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E21\u0E35\u0E1B\u0E23\u0E30\u0E2A\u0E34\
  \u0E17\u0E18\u0E34\u0E1C\u0E25 \u0E04\u0E38\u0E13\u0E15\u0E49\u0E2D\u0E07\u0E21\u0E35\
  \u0E2B\u0E19\u0E49\u0E32\u0E15\u0E48\u0E32\u0E07 Immediate \u0E40\u0E1B\u0E34\u0E14\
  \u0E2D\u0E22\u0E39\u0E48 (\u0E21\u0E38\u0E21\u0E21\u0E2D\u0E07 > \u0E2B\u0E19\u0E49\
  \u0E32\u0E15\u0E48\u0E32\u0E07 Immediate \u0E2B\u0E23\u0E37\u0E2D\u0E01\u0E14 `Ctrl+G`\
  \ \u0E43\u0E19 VBE) \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\
  \u0E22\u0E48\u0E32\u0E07\u0E07\u0E48\u0E32\u0E22\u0E46 \u0E02\u0E2D\u0E07\u0E01\u0E32\
  \u0E23\u0E43\u0E0A\u0E49 `Debug.Print` \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E41\u0E2A\
  \u0E14\u0E07\u0E04\u0E48\u0E32\u0E02\u0E2D\u0E07\u0E15\u0E31\u0E27\u0E41\u0E1B\u0E23\
  \u0E41\u0E25\u0E30\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E17\u0E35\u0E48\u0E01\
  \u0E33\u0E2B\u0E19\u0E14\u0E40\u0E2D\u0E07."
title: "\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E1C\u0E25\u0E25\u0E31\u0E1E\
  \u0E18\u0E4C\u0E01\u0E32\u0E23\u0E41\u0E01\u0E49\u0E44\u0E02\u0E42\u0E04\u0E49\u0E14"
weight: 33
---

## วิธีการ:
ใน VBA, การใช้คำสั่ง `Debug.Print` คือหัวใจหลักสำหรับการพิมพ์ข้อมูลดีบักไปยังหน้าต่าง Immediate ใน Visual Basic Editor (VBE) เพื่อใช้ฟีเจอร์นี้ได้อย่างมีประสิทธิผล คุณต้องมีหน้าต่าง Immediate เปิดอยู่ (มุมมอง > หน้าต่าง Immediate หรือกด `Ctrl+G` ใน VBE)

นี่คือตัวอย่างง่ายๆ ของการใช้ `Debug.Print` เพื่อแสดงค่าของตัวแปรและข้อความที่กำหนดเอง:

```basic
Sub PrintDebugInfo()
    Dim sampleVar As Integer
    sampleVar = 42
    Debug.Print "The value of sampleVar is: "; sampleVar
End Sub
```

เมื่อคุณรัน subroutine นี้ หน้าต่าง Immediate จะแสดง:
```
The value of sampleVar is: 42
```

คุณยังสามารถใช้งานนี้เพื่อติดตามการทำงานของโลจิกเงื่อนไขที่ซับซ้อนโดยการแทรกคำสั่ง `Debug.Print` ในสาขาต่างๆ ของโค้ดของคุณ:

```basic
Sub CheckValue()
    Dim valueToCheck As Integer
    valueToCheck = 9
    
    If valueToCheck > 10 Then
        Debug.Print "Value is greater than 10."
    ElseIf valueToCheck < 10 And valueToCheck > 0 Then
        Debug.Print "Value is between 1 and 9."
    Else
        Debug.Print "Value is 10 or less than 1."
    End If
End Sub
```

การรัน `CheckValue` ผลลัพธ์ที่ได้คือ:
```
Value is between 1 and 9.
```

จำไว้ว่า ผลลัพธ์จาก `Debug.Print` จะไปที่หน้าต่าง Immediate เพียงแห่งเดียว ซึ่งมีประโยชน์มากในระหว่างขั้นตอนการพัฒนา แต่จะไม่ปรากฏในส่วนที่เผชิญหน้ากับผู้ใช้ของแอปพลิเคชัน

## การทำความเข้าใจลึกซึ้ง
หน้าต่าง Immediate และวิธีการ `Debug.Print` มีรากฐานอันยาวนานในประวัติศาสตร์ของ Visual Basic for Applications สะท้อนถึงการพัฒนาแนวปฏิบัติการดีบักเมื่อเวลาผ่านไป ในแต่ละปี ด้วยการพัฒนาสิ่งแวดล้อมการพัฒนาที่เป็นไปอย่างรวดเร็ว ก็ได้มีการนำเสนอเครื่องมือดีบักที่ทันสมัยยิ่งขึ้นเช่นจุดหยุดการทำงาน การดูค่าตัวแปรระหว่างการทำงาน และเครื่องมือที่มีความซับซ้อนมากขึ้น ซึ่งมอบข้อมูลอย่างใกล้ชิดและทันทีเกี่ยวกับพฤติกรรมของโค้ด

อย่างไรก็ตาม `Debug.Print` และหน้าต่าง Immediate ยังคงเป็นเครื่องมือที่มีคุณค่าอย่างมาก โดยเฉพาะสำหรับการดีบักอย่างรวดเร็วหรือเมื่อมีการจัดการกับโค้ดที่ทำให้การหยุดชะงักได้ยาก (เช่น ตัวจัดการเหตุการณ์) นั้นกล่าวได้ว่า การพึ่งพาระบบพิมพ์เดียวสำหรับการดีบักในการเขียนโปรแกรมสมัยใหม่อาจมีประสิทธิภาพน้อยกว่าการใช้งานดีบักเกอร์ที่รวมอยู่ในตัว ที่มีความสามารถในการตรวจสอบจุดหยุด การเฝ้าดู และการตรวจสอบสแต็ก

การใช้ฟีเจอร์อื่นๆ เช่น กรอบการทำงานสำหรับการให้บันทึกหรือเครื่องมือดีบักที่มีความสามารถมากขึ้น อาจนำเสนอคุณสมบัติและความยืดหยุ่นที่เพิ่มขึ้น การใช้ `Debug.Print` ใน VBA นั้นยังคงเป็นเครื่องมือที่มีค่าอย่างมาก โดยเฉพาะสำหรับโปรแกรมเมอร์ที่ถ่ายโอนมาจากภาษาอื่นซึ่งมีความคุ้นเคยกับเทคนิคการดีบักที่ใช้การพิมพ์ อย่างไรก็ตาม เมื่อพวกเขามีความคุ้นเคยมากขึ้นกับ VBA และ Visual Basic Editor การสำรวจชุดเครื่องมือดีบักที่มีให้ครบถ้วนสามารถนำไปสู่การแก้ไขปัญหาที่มีประสิทธิภาพและมีประสิทธิผลมากขึ้น
