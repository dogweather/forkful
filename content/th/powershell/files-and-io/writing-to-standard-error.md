---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:54:38.232842-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: PowerShell \u0E17\u0E33\u0E43\
  \u0E2B\u0E49\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\
  \u0E07 stderr \u0E07\u0E48\u0E32\u0E22\u0E02\u0E36\u0E49\u0E19\u0E1C\u0E48\u0E32\
  \u0E19\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 cmdlet `Write-Error` \u0E2B\u0E23\u0E37\
  \u0E2D\u0E42\u0E14\u0E22\u0E01\u0E32\u0E23\u0E19\u0E33\u0E2D\u0E2D\u0E01\u0E44\u0E1B\
  \u0E22\u0E31\u0E07\u0E40\u0E21\u0E18\u0E2D\u0E14 `$host.ui.WriteErrorLine()` \u0E2D\
  \u0E22\u0E48\u0E32\u0E07\u0E44\u0E23\u0E01\u0E47\u0E15\u0E32\u0E21\u2026"
lastmod: '2024-04-05T21:54:02.276855-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\
  \u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\u0E07 stderr \u0E07\u0E48\u0E32\u0E22\u0E02\
  \u0E36\u0E49\u0E19\u0E1C\u0E48\u0E32\u0E19\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 cmdlet\
  \ `Write-Error` \u0E2B\u0E23\u0E37\u0E2D\u0E42\u0E14\u0E22\u0E01\u0E32\u0E23\u0E19\
  \u0E33\u0E2D\u0E2D\u0E01\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E40\u0E21\u0E18\u0E2D\u0E14\
  \ `$host.ui.WriteErrorLine()` \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E44\u0E23\u0E01\u0E47\
  \u0E15\u0E32\u0E21 \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E40\u0E1B\
  \u0E25\u0E35\u0E48\u0E22\u0E19\u0E17\u0E34\u0E28\u0E17\u0E32\u0E07 stderr \u0E42\
  \u0E14\u0E22\u0E15\u0E23\u0E07 \u0E04\u0E38\u0E13\u0E2D\u0E32\u0E08\u0E08\u0E30\u0E0A\
  \u0E2D\u0E1A\u0E43\u0E0A\u0E49\u0E40\u0E21\u0E18\u0E2D\u0E14\u0E02\u0E2D\u0E07 .NET\
  \ \u0E2B\u0E23\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\
  \u0E19\u0E17\u0E34\u0E28\u0E17\u0E32\u0E07\u0E15\u0E31\u0E27\u0E01\u0E33\u0E2B\u0E19\
  \u0E14\u0E44\u0E1F\u0E25\u0E4C\u0E17\u0E35\u0E48 PowerShell \u0E40\u0E2A\u0E19\u0E2D\
  \ **\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E17\u0E35\u0E48 1:** \u0E43\
  \u0E0A\u0E49 `Write-Error` \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E02\u0E35\u0E22\
  \u0E19\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\
  \u0E14\u0E44\u0E1B\u0E22\u0E31\u0E07 stderr."
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1B\u0E22\u0E31\u0E07\
  \u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E21\u0E32\u0E15\u0E23\
  \u0E10\u0E32\u0E19"
weight: 25
---

## วิธีการ:
PowerShell ทำให้การเขียนไปยัง stderr ง่ายขึ้นผ่านการใช้ cmdlet `Write-Error` หรือโดยการนำออกไปยังเมธอด `$host.ui.WriteErrorLine()` อย่างไรก็ตาม สำหรับการเปลี่ยนทิศทาง stderr โดยตรง คุณอาจจะชอบใช้เมธอดของ .NET หรือการเปลี่ยนทิศทางตัวกำหนดไฟล์ที่ PowerShell เสนอ

**ตัวอย่างที่ 1:** ใช้ `Write-Error` เพื่อเขียนข้อความผิดพลาดไปยัง stderr

```powershell
Write-Error "This is an error message."
```

ผลลัพธ์ที่ stderr:
```
Write-Error: This is an error message.
```

**ตัวอย่างที่ 2:** ใช้ `$host.ui.WriteErrorLine()` สำหรับการเขียน stderr โดยตรง

```powershell
$host.ui.WriteErrorLine("Direct stderr write.")
```

ผลลัพธ์ที่ stderr:
```
Direct stderr write.
```

**ตัวอย่างที่ 3:** ใช้เมธอดของ .NET สำหรับการเขียนไปยัง stderr

```powershell
[Console]::Error.WriteLine("Using .NET method for stderr")
```

ผลลัพธ์ของเมธอดนี้:
```
Using .NET method for stderr
```

**ตัวอย่างที่ 4:** การเปลี่ยนทิศทางผลลัพธ์ผิดพลาดโดยใช้ตัวกำหนดไฟล์ `2>`

ตัวกำหนดไฟล์ใน PowerShell สามารถเปลี่ยนทิศทางสตรีมต่างๆ สำหรับ stderr ตัวกำหนดไฟล์คือ `2` นี่คือตัวอย่างของการเปลี่ยนทิศทาง stderr ไปยังไฟล์ที่ชื่อว่า `error.log` ขณะที่ดำเนินคำสั่งที่สร้างข้อผิดพลาด

```powershell
Get-Item NonExistentFile.txt 2> error.log
```

ตัวอย่างนี้ไม่ผลิตผลลัพธ์ของคอนโซล แต่สร้างไฟล์ `error.log` ในไดเร็กทอรีปัจจุบันที่มีข้อความผิดพลาดจากการพยายามเข้าถึงไฟล์ที่ไม่มีอยู่

โดยสรุป PowerShell ให้วิธีการหลายวิธีในการเขียนและจัดการผลลัพธ์ข้อผิดพลาดอย่างมีประสิทธิภาพ ช่วยให้สามารถจัดการข้อผิดพลาดและกลยุทธ์การบันทึกในสคริปต์และแอปพลิเคชันได้อย่างชาญฉลาด
