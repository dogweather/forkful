---
changelog:
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 18:04:24.388101-07:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: PowerShell \u0E17\u0E33\u0E43\
  \u0E2B\u0E49\u0E01\u0E32\u0E23\u0E40\u0E23\u0E34\u0E48\u0E21\u0E42\u0E1B\u0E23\u0E40\
  \u0E08\u0E01\u0E15\u0E4C\u0E43\u0E2B\u0E21\u0E48\u0E40\u0E1B\u0E47\u0E19\u0E40\u0E23\
  \u0E37\u0E48\u0E2D\u0E07\u0E07\u0E48\u0E32\u0E22\u0E14\u0E32\u0E22 \u0E04\u0E38\u0E13\
  \u0E2D\u0E32\u0E08\u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\
  \u0E07\u0E44\u0E14\u0E40\u0E23\u0E47\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E42\u0E1B\u0E23\u0E40\u0E08\u0E01\u0E15\u0E4C\u0E02\u0E2D\u0E07\
  \u0E04\u0E38\u0E13\u0E41\u0E25\u0E30\u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32 git\
  \ repository \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33\
  ."
lastmod: '2024-04-04T00:27:17.009334-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E01\u0E32\u0E23\u0E40\u0E23\u0E34\
  \u0E48\u0E21\u0E42\u0E1B\u0E23\u0E40\u0E08\u0E01\u0E15\u0E4C\u0E43\u0E2B\u0E21\u0E48\
  \u0E40\u0E1B\u0E47\u0E19\u0E40\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E07\u0E48\u0E32\u0E22\
  \u0E14\u0E32\u0E22 \u0E04\u0E38\u0E13\u0E2D\u0E32\u0E08\u0E15\u0E49\u0E2D\u0E07\u0E01\
  \u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E44\u0E14\u0E40\u0E23\u0E47\u0E01\u0E17\
  \u0E2D\u0E23\u0E35\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E42\u0E1B\u0E23\u0E40\u0E08\
  \u0E01\u0E15\u0E4C\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E41\u0E25\u0E30\u0E15\u0E31\
  \u0E49\u0E07\u0E04\u0E48\u0E32 git repository \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\
  \u0E27\u0E34\u0E18\u0E35\u0E17\u0E33."
title: "\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E42\u0E1B\u0E23\u0E40\u0E08\
  \u0E47\u0E04\u0E43\u0E2B\u0E21\u0E48"
weight: 1
---

## วิธีการ:
PowerShell ทำให้การเริ่มโปรเจกต์ใหม่เป็นเรื่องง่ายดาย คุณอาจต้องการสร้างไดเร็กทอรีสำหรับโปรเจกต์ของคุณและตั้งค่า git repository นี่คือวิธีทำ:

```PowerShell
# สร้างไดเร็กทอรีใหม่สำหรับโปรเจกต์ของคุณ
New-Item -Path 'C:\MyProjects\NewCoolApp' -ItemType Directory

# ไปยังไดเร็กทอรีใหม่ของคุณ
Set-Location -Path 'C:\MyProjects\NewCoolApp'

# ตั้งค่า git repository ใหม่ ถ้าคุณใช้การควบคุมเวอร์ชัน
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

## ศึกษาเพิ่มเติม
PowerShell เป็นภาษาสคริปต์หลักสำหรับการอัตโนมัติใน Windows นับตั้งแต่เปิดตัวในปี 2006 การสร้างโปรเจกต์ใหม่ด้วย PowerShell ไม่เพียงแค่เกี่ยวกับการสร้างไดเร็กทอรีเท่านั้น แต่ยังเป็นกระบวนการในการกำหนดขอบเขตโปรเจกต์, นิยามสคริปต์ หรือเตรียมงานอัตโนมัติ

แม้ว่า PowerShell เป็นที่นิยมในโลก Windows, ผู้ใช้ Unix-like มักพึ่งพา 'bash' หรือ 'zsh' สำหรับงานที่คล้ายกัน อย่างไรก็ตาม ด้วยการเกิดขึ้นของ PowerShell Core, PowerShell ได้ก้าวเข้าสู่แหวนหลายแพลตฟอร์ม ช่วยให้สามารถเขียนสคริปต์และอัตโนมัติข้ามแพลตฟอร์มได้

ซึ่งฝังลึกในการออกแบบของ PowerShell คือคุณสมบัติที่เป็นวัตถุ โดยใช้ cmdlets (ออกเสียงว่า คอมมานด์-เลต) ที่ส่งออกวัตถุ Cmdlets เช่น `New-Item` ไม่เพียงแค่สร้างไฟล์หรือโฟลเดอร์เท่านั้น แต่ยังสร้างวัตถุที่สคริปต์ของคุณสามารถโต้ตอบได้ การตั้งค่าโปรเจกต์ใหม่อาจรวมถึงการกำหนดโครงสร้างโฟลเดอร์, สร้างไฟล์ README, ตั้งค่าไฟล์ .gitignore หรือแม้กระทั่งเทมเพลตไฟล์โค้ดเริ่มต้น

การใช้งานการตั้งค่าโปรเจกต์ใน PowerShell อาจใช้ cmdlets จำนวนมาก ตั้งแต่การจัดการไฟล์ (`New-Item`) ไปจนถึงการกำหนดค่าสภาพแวดล้อม (`Set-Location`) การรวมเหล่านี้กับความสามารถในการเขียนสคริปต์ของ PowerShell สามารถสร้างสคริปต์การตั้งค่าที่ทรงพลังซึ่งทำหน้าที่เป็นจุดเริ่มต้นของโปรเจกต์ ทำให้โครงสร้างพื้นฐานของโปรเจกต์ของคุณเสร็จสมบูรณ์ด้วยความยุ่งยากน้อยที่สุด

## ดูเพิ่มเติม
- [PowerShell Scripting](https://docs.microsoft.com/en-us/powershell/scripting/overview)
- [Pro Git Book](https://git-scm.com/book/en/v2)
- [GitHub's Hello World](https://guides.github.com/activities/hello-world/)
- [PowerShell Core on GitHub](https://github.com/PowerShell/PowerShell)
