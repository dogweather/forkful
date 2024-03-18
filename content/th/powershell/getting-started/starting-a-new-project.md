---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:28.741443-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E42\
  \u0E1B\u0E23\u0E40\u0E08\u0E01\u0E15\u0E4C\u0E43\u0E2B\u0E21\u0E48\u0E2B\u0E21\u0E32\
  \u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E27\u0E32\u0E07\u0E23\u0E32\u0E01\u0E10\
  \u0E32\u0E19\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E1C\u0E25\u0E07\u0E32\u0E19\u0E01\
  \u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\
  \u0E04\u0E38\u0E13 PowerShell \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E01\u0E32\u0E23\u0E40\
  \u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E42\u0E1B\u0E23\u0E40\u0E08\u0E01\u0E15\
  \u0E4C\u0E43\u0E2B\u0E21\u0E48\u0E40\u0E1B\u0E47\u0E19\u0E40\u0E23\u0E37\u0E48\u0E2D\
  \u0E07\u0E07\u0E48\u0E32\u0E22\u2026"
lastmod: '2024-03-17T21:57:56.442145-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E42\
  \u0E1B\u0E23\u0E40\u0E08\u0E01\u0E15\u0E4C\u0E43\u0E2B\u0E21\u0E48\u0E2B\u0E21\u0E32\
  \u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E27\u0E32\u0E07\u0E23\u0E32\u0E01\u0E10\
  \u0E32\u0E19\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E1C\u0E25\u0E07\u0E32\u0E19\u0E01\
  \u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\
  \u0E04\u0E38\u0E13 PowerShell \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E01\u0E32\u0E23\u0E40\
  \u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E42\u0E1B\u0E23\u0E40\u0E08\u0E01\u0E15\
  \u0E4C\u0E43\u0E2B\u0E21\u0E48\u0E40\u0E1B\u0E47\u0E19\u0E40\u0E23\u0E37\u0E48\u0E2D\
  \u0E07\u0E07\u0E48\u0E32\u0E22\u2026"
title: "\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E42\u0E04\u0E23\u0E07\u0E01\
  \u0E32\u0E23\u0E43\u0E2B\u0E21\u0E48"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การเริ่มต้นโปรเจกต์ใหม่หมายถึงการวางรากฐานสำหรับผลงานการเขียนโค้ดของคุณ PowerShell ทำให้การเริ่มต้นโปรเจกต์ใหม่เป็นเรื่องง่าย คุณอาจต้องการสร้างไดเร็กทอรี่สำหรับโปรเจกต์ของคุณและตั้งค่าพื้นที่เก็บข้อมูล git นี่คือวิธีการ:

```PowerShell
# สร้างไดเร็กทอรี่ใหม่สำหรับโปรเจกต์ของคุณ
New-Item -Path 'C:\MyProjects\NewCoolApp' -ItemType Directory

# นำทางไปยังไดเร็กทอรี่ใหม่ของคุณ
Set-Location -Path 'C:\MyProjects\NewCoolApp'

# เริ่มต้นพื้นที่เก็บข้อมูล git ใหม่หากคุณใช้การควบคุมเวอร์ชัน
git init
```

ตัวอย่างผลลัพธ์:
```
    Directory: C:\MyProjects

Mode                 LastWriteTime         Length Name
----                 -------------         ------ ----
d-----          1/1/2023   12:00 AM                NewCoolApp
Initialized empty Git repository in C:/MyProjects/NewCoolApp/.git/
```

## ดำดิ่งลึกลงไป
PowerShell เป็นภาษาสคริปต์ที่ใช้ในการอัตโนมัติ Windows ตั้งแต่เปิดตัวในปี 2006 การสร้างโปรเจกต์ใหม่ด้วย PowerShell ไม่เพียงแต่เกี่ยวกับการสร้างไดเร็กทอรี่เท่านั้น แต่เป็นพิธีกรรมสำหรับการกำหนดขอบเขตโปรเจกต์ การกำหนดสคริปต์ หรือการเตรียมการอัตโนมัติงานต่างๆ

ในขณะที่ PowerShell เป็นที่นิยมในโลกของ Windows ผู้ใช้ที่คล้ายกับ Unix มักพึ่งพา 'bash' หรือ 'zsh' สำหรับงานที่คล้ายกัน อย่างไรก็ตาม ด้วยการเปิดตัวของ PowerShell Core PowerShell ได้ก้าวเข้าสู่สังเวียนแพลตฟอร์มหลายระบบ ช่วยให้สามารถสคริปต์และอัตโนมัติได้ข้ามแพลตฟอร์ม

ที่ลึกซึ้งในการออกแบบของ PowerShell คือธรรมชาติที่เป็นวัตถุ โดยใช้ cmdlets (ออกเสียงว่า คำเมนด์-เล็ตส์) ที่ส่งออกวัตถุ Cmdlets เช่น `New-Item` ไม่เพียงแต่สร้างไฟล์หรือโฟลเดอร์เท่านั้น แต่ยังก่อสร้างวัตถุที่สคริปต์ของคุณสามารถโต้ตอบได้ การตั้งค่าโปรเจกต์ใหม่อาจรวมถึงการสร้างโครงสร้างโฟลเดอร์ การสร้างไฟล์ README การตั้งค่าไฟล์ .gitignore หรือแม้แต่การเทมเพลตไฟล์โค้ดเริ่มต้น

การใช้ PowerShell ในการตั้งค่าโปรเจกต์อาจใช้ cmdlets จำนวนมาก ตั้งแต่การจัดการไฟล์ (`New-Item`) ไปจนถึงการกำหนดค่าสภาพแวดล้อม (`Set-Location`) การผสานสิ่งเหล่านี้เข้ากับความสามารถในการสคริปต์ของ PowerShell สามารถสร้างสคริปต์การตั้งค่าที่ทรงพลังซึ่งทำหน้าที่เป็นตัวเริ่มต้นโปรเจกต์ ออกแบบโครงสร้างของโปรเจกต์ของคุณด้วยความยุ่งยากน้อยที่สุด

## ดูเพิ่มเติม
- [การสคริปต์ด้วย PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/overview)
- [หนังสือ Pro Git](https://git-scm.com/book/en/v2)
- [Hello World ของ GitHub](https://guides.github.com/activities/hello-world/)
- [PowerShell Core บน GitHub](https://github.com/PowerShell/PowerShell)
