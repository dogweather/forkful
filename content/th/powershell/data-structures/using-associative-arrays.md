---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:53.971551-06:00
description: "Associative arrays \u0E2B\u0E23\u0E37\u0E2D\u0E17\u0E35\u0E48\u0E23\u0E39\
  \u0E49\u0E08\u0E31\u0E01\u0E43\u0E19\u0E0A\u0E37\u0E48\u0E2D hash tables \u0E2B\u0E23\
  \u0E37\u0E2D dictionaries \u0E43\u0E19 PowerShell \u0E0A\u0E48\u0E27\u0E22\u0E43\
  \u0E2B\u0E49\u0E04\u0E38\u0E13\u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\
  \u0E25\u0E43\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E04\u0E39\u0E48\u0E02\u0E2D\
  \u0E07\u0E04\u0E35\u0E22\u0E4C\u0E41\u0E25\u0E30\u0E04\u0E48\u0E32 (key-value pairs)\u2026"
lastmod: '2024-03-17T21:57:56.432258-06:00'
model: gpt-4-0125-preview
summary: "Associative arrays \u0E2B\u0E23\u0E37\u0E2D\u0E17\u0E35\u0E48\u0E23\u0E39\
  \u0E49\u0E08\u0E31\u0E01\u0E43\u0E19\u0E0A\u0E37\u0E48\u0E2D hash tables \u0E2B\u0E23\
  \u0E37\u0E2D dictionaries \u0E43\u0E19 PowerShell \u0E0A\u0E48\u0E27\u0E22\u0E43\
  \u0E2B\u0E49\u0E04\u0E38\u0E13\u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\
  \u0E25\u0E43\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E04\u0E39\u0E48\u0E02\u0E2D\
  \u0E07\u0E04\u0E35\u0E22\u0E4C\u0E41\u0E25\u0E30\u0E04\u0E48\u0E32 (key-value pairs)\
  \ \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E01\u0E32\u0E23\u0E40\u0E23\u0E35\u0E22\u0E01\
  \u0E14\u0E39\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E40\u0E1B\u0E47\u0E19\u0E40\u0E23\
  \u0E37\u0E48\u0E2D\u0E07\u0E07\u0E48\u0E32\u0E22\u0E41\u0E25\u0E30\u0E21\u0E35\u0E1B\
  \u0E23\u0E30\u0E2A\u0E34\u0E17\u0E18\u0E34\u0E20\u0E32\u0E1E \u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E43\u0E0A\u0E49\u0E1E\u0E27\u0E01\
  \u0E21\u0E31\u0E19\u0E43\u0E19\u0E01\u0E32\u0E23\u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E40\u0E01\u0E35\u0E48\u0E22\u0E27\u0E02\
  \u0E49\u0E2D\u0E07\u0E01\u0E31\u0E19\u0E44\u0E27\u0E49\u0E14\u0E49\u0E27\u0E22\u0E01\
  \u0E31\u0E19\u0E43\u0E19\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21\
  \u0E32\u0E23\u0E16\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u0E44\u0E14\u0E49\u0E07\
  \u0E48\u0E32\u0E22\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E04\u0E35\u0E22\u0E4C."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E41\u0E2D\u0E40\u0E23\u0E22\u0E4C\u0E2A\
  \u0E21\u0E32\u0E0A\u0E34\u0E01"
weight: 15
---

## อะไรและทำไม?

Associative arrays หรือที่รู้จักในชื่อ hash tables หรือ dictionaries ใน PowerShell ช่วยให้คุณเก็บข้อมูลในรูปแบบคู่ของคีย์และค่า (key-value pairs) ทำให้การเรียกดูข้อมูลเป็นเรื่องง่ายและมีประสิทธิภาพ โปรแกรมเมอร์ใช้พวกมันในการเก็บข้อมูลที่เกี่ยวข้องกันไว้ด้วยกันในวิธีที่สามารถเข้าถึงได้ง่ายโดยใช้คีย์

## วิธีใช้:

การสร้างและใช้ associative arrays ใน PowerShell นั้นค่อนข้างตรงไปตรงมา นี่คือวิธีทำมัน:

**การสร้าง associative array:**

```PowerShell
$myAssociativeArray = @{}
$myAssociativeArray["name"] = "Alex"
$myAssociativeArray["age"] = 25
$myAssociativeArray["job"] = "Engineer"
```

โค้ดข้างต้นสร้าง associative array ที่มีคู่คีย์-ค่าสามคู่

**การเข้าถึงค่า:**

เพื่อดึงค่าออกมา ให้อ้างอิงคีย์ของมัน:

```PowerShell
Write-Output $myAssociativeArray["name"]
```

**ตัวอย่างผลลัพธ์:**

```
Alex
```

**การเพิ่มหรือแก้ไขข้อมูล:**

เพียงใช้คีย์ในการเพิ่มคู่ใหม่หรือแก้ไขคู่ที่มีอยู่:

```PowerShell
$myAssociativeArray["location"] = "New York" # เพิ่มคู่คีย์-ค่าใหม่
$myAssociativeArray["job"] = "Senior Engineer" # แก้ไขคู่ที่มีอยู่
```

**การวนซ้ำผ่าน associative array:**

วนลูปผ่านคีย์และค่าแบบนี้:

```PowerShell
foreach ($key in $myAssociativeArray.Keys) {
  $value = $myAssociativeArray[$key]
  Write-Output "$key : $value"
}
```

**ตัวอย่างผลลัพธ์:**

```
name : Alex
age : 25
job : Senior Engineer
location : New York
```

## ลงลึก

คอนเซปต์ของ associative arrays เป็นที่นิยมในหลายภาษาการเขียนโปรแกรม โดยมักจะถูกเรียกว่า dictionary, map, หรือ hash table ตามภาษาที่ใช้ ใน PowerShell, associative arrays ถูกประยุกต์ใช้เป็น hash tables ซึ่งมีประสิทธิภาพสูงสำหรับการค้นหาคีย์, เก็บข้อมูล, และรักษาคอลเลกชันของคีย์ที่ไม่ซ้ำกัน

ตามประวัติศาสตร์แล้ว, associative arrays ให้วิธีการจัดการคอลเลกชันของวัตถุที่แต่ละรายการสามารถเรียกข้อมูลออกมาได้อย่างรวดเร็วโดยไม่ต้องวนซ้ำผ่านทั้งคอลเลกชันโดยใช้คีย์ ประสิทธิภาพในการเรียกข้อมูลและการแก้ไขข้อมูลใน associative arrays ทำให้พวกมันเป็นทางเลือกที่ต้องการสำหรับงานต่างๆ อย่างไรก็ตาม พวกเขามีข้อจำกัด เช่น การรักษาลำดับ ซึ่ง ordered dictionaries หรือวัตถุที่กำหนดเองอาจเป็นทางเลือกที่ดีกว่า

ทั้งนี้ associative arrays/hash tables ใน PowerShell มีความยืดหยุ่นและเป็นเครื่องมือที่มีพลังสำหรับการเขียนสคริปต์อย่างมาก พวกเขาช่วยให้การเก็บข้อมูลแบบไดนามิกและเป็นที่นิยมใช้ในการกำหนดค่า การจัดการข้อมูล และทุกที่ที่ต้องการรูปแบบข้อมูลที่มีโครงสร้างโดยไม่ต้องใช้การกำหนดคลาสอย่างเป็นทางการ เพียงจำไว้ ในขณะที่ associative arrays เหมาะสำหรับการเรียกข้อมูลโดยใช้คีย์ ถ้างานของคุณเกี่ยวข้องกับโครงสร้างข้อมูลที่ซับซ้อนหรือต้องการรักษาลำดับเฉพาะ คุณอาจต้องการสำรวจประเภทข้อมูลอื่นหรือวัตถุที่กำหนดเองภายใน PowerShell
