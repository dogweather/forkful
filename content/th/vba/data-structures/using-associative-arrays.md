---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:20.890945-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E43\u0E19 VBA, \u0E27\
  \u0E31\u0E15\u0E16\u0E38 `Dictionary` \u0E43\u0E2B\u0E49\u0E1F\u0E31\u0E07\u0E01\
  \u0E4C\u0E0A\u0E31\u0E19\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E17\u0E35\
  \u0E48\u0E04\u0E25\u0E49\u0E32\u0E22\u0E01\u0E31\u0E1A\u0E2D\u0E32\u0E23\u0E4C\u0E40\
  \u0E23\u0E22\u0E4C\u0E41\u0E1A\u0E1A\u0E2A\u0E2B\u0E2A\u0E31\u0E21\u0E1E\u0E31\u0E19\
  \u0E18\u0E4C \u0E04\u0E38\u0E13\u0E15\u0E49\u0E2D\u0E07\u0E40\u0E1E\u0E34\u0E48\u0E21\
  \u0E01\u0E32\u0E23\u0E2D\u0E49\u0E32\u0E07\u0E2D\u0E34\u0E07\u0E16\u0E36\u0E07 Microsoft\
  \ Scripting Runtime \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E43\u0E0A\u0E49\u0E07\u0E32\
  \u0E19:\u2026"
lastmod: '2024-03-17T21:57:56.030466-06:00'
model: gpt-4-0125-preview
summary: "\u0E43\u0E19 VBA, \u0E27\u0E31\u0E15\u0E16\u0E38 `Dictionary` \u0E43\u0E2B\
  \u0E49\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E01\u0E32\u0E23\u0E17\u0E33\
  \u0E07\u0E32\u0E19\u0E17\u0E35\u0E48\u0E04\u0E25\u0E49\u0E32\u0E22\u0E01\u0E31\u0E1A\
  \u0E2D\u0E32\u0E23\u0E4C\u0E40\u0E23\u0E22\u0E4C\u0E41\u0E1A\u0E1A\u0E2A\u0E2B\u0E2A\
  \u0E31\u0E21\u0E1E\u0E31\u0E19\u0E18\u0E4C \u0E04\u0E38\u0E13\u0E15\u0E49\u0E2D\u0E07\
  \u0E40\u0E1E\u0E34\u0E48\u0E21\u0E01\u0E32\u0E23\u0E2D\u0E49\u0E32\u0E07\u0E2D\u0E34\
  \u0E07\u0E16\u0E36\u0E07 Microsoft Scripting Runtime \u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E41\u0E2D\u0E40\u0E23\u0E22\u0E4C\u0E2A\
  \u0E21\u0E32\u0E0A\u0E34\u0E01"
weight: 15
---

## วิธีการ:
ใน VBA, วัตถุ `Dictionary` ให้ฟังก์ชันการทำงานที่คล้ายกับอาร์เรย์แบบสหสัมพันธ์ คุณต้องเพิ่มการอ้างอิงถึง Microsoft Scripting Runtime เพื่อใช้งาน:

1. ในตัวแก้ไข VBA, ไปที่ Tools > References...
2. ตรวจสอบ "Microsoft Scripting Runtime" แล้วคลิก OK

นี่คือวิธีการประกาศ, เติมข้อมูล, และเข้าถึงอิเท็มใน `Dictionary`:

```vb
Dim sampleDictionary As Dictionary
Set sampleDictionary = New Dictionary

' เพิ่มอิเท็ม
sampleDictionary.Add Key:="Name", Item:="John Doe"
sampleDictionary.Add Key:="Age", Item:=29
sampleDictionary.Add Key:="Occupation", Item:="Engineer"

' การเข้าถึงอิเท็ม
Debug.Print sampleDictionary.Item("Name")  ' ผลลัพธ์: John Doe
Debug.Print sampleDictionary.Item("Age")   ' ผลลัพธ์: 29

' การตรวจสอบว่ามีคีย์อยู่หรือไม่
If sampleDictionary.Exists("Occupation") Then
    Debug.Print "Occupation Key Exists"
End If

' การลบอิเท็ม
sampleDictionary.Remove("Occupation")

' การวนรอบไดค์ชันนารี
For Each Key In sampleDictionary.Keys
    Debug.Print Key & ": " & sampleDictionary.Item(Key)
Next Key
```

## แง่ลึก
วัตถุ `Dictionary` ทำงานร่วมกับส่วนประกอบของ Windows Scripting Host ในลักษณะเดียวกัน ดังนั้น มันเป็นวัตถุ COM ที่เชื่อมโยงช้า ซึ่งเป็นวิธีทั่วไปในการขยายฟังก์ชันการทำงานของ VBA ในอดีต การใช้งานของมันใน VBA สามารถเพิ่มความสามารถของภาษาในการจัดการชุดข้อมูลที่ซับซ้อนโดยไม่ต้องบังคับให้มีโครงสร้างที่แข็งกระด้าง เช่นเดียวกับในอาร์เรย์แบบดั้งเดิมหรือช่วงของ Excel

ข้อจำกัดหนึ่งที่ควรจำไว้คือ การเข้าถึง `Dictionary` ต้องมีการตั้งค่าการอ้างอิงถึง Microsoft Scripting Runtime ซึ่งอาจทำให้การแจกจ่ายโปรเจกต์ VBA ของคุณซับซ้อนขึ้น ตัวเลือกอื่น ๆ เช่น Collections มีอยู่ภายใน VBA แต่ขาดคุณสมบัติบางอย่างของ `Dictionary` เช่น ความสามารถในการตรวจสอบการมีอยู่ของคีย์โดยไม่ทำให้เกิดข้อผิดพลาด

ในบริบทการเขียนโปรแกรมล่าสุด ภาษาอย่าง Python ให้การสนับสนุนฟีเจอร์อาร์เรย์แบบสหสัมพันธ์ (รู้จักในชื่อ dictionaries ใน Python เช่นกัน) โดยไม่ต้องเพิ่มการอ้างอิงภายนอก การสนับสนุนนี้ทำให้กระบวนการง่ายขึ้นและเสนอคุณสมบัติขั้นสูงออกมาจากกล่อง อย่างไรก็ตาม ในขอบเขตของ VBA และสำหรับแอปพลิเคชั่นที่เฉพาะเจาะจงเพื่อการทำงานอัตโนมัติในชุด Microsoft Office การใช้วัตถุ `Dictionary` ยังคงเป็นวิธีการที่มีพลังและเกี่ยวข้องสำหรับโครงสร้างข้อมูลแบบอาร์เรย์สหสัมพันธ์
