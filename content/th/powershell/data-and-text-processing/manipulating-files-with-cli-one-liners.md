---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:38.082101-06:00
description: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E44\u0E1F\u0E25\
  \u0E4C\u0E14\u0E49\u0E27\u0E22 CLI one-liners \u0E43\u0E19 PowerShell \u0E04\u0E37\
  \u0E2D\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E41\u0E1B\u0E25\
  \u0E07, \u0E22\u0E49\u0E32\u0E22 \u0E2B\u0E23\u0E37\u0E2D \u0E40\u0E23\u0E35\u0E22\
  \u0E01\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E44\u0E1F\u0E25\u0E4C\u0E44\u0E14\u0E49\
  \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\u0E27\u0E42\u0E14\
  \u0E22\u0E15\u0E23\u0E07\u0E08\u0E32\u0E01 command line\u2026"
lastmod: '2024-03-17T21:57:56.436370-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E44\u0E1F\u0E25\
  \u0E4C\u0E14\u0E49\u0E27\u0E22 CLI one-liners \u0E43\u0E19 PowerShell \u0E04\u0E37\
  \u0E2D\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E41\u0E1B\u0E25\
  \u0E07, \u0E22\u0E49\u0E32\u0E22 \u0E2B\u0E23\u0E37\u0E2D \u0E40\u0E23\u0E35\u0E22\
  \u0E01\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E44\u0E1F\u0E25\u0E4C\u0E44\u0E14\u0E49\
  \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\u0E27\u0E42\u0E14\
  \u0E22\u0E15\u0E23\u0E07\u0E08\u0E32\u0E01 command line \u0E19\u0E31\u0E01\u0E42\
  \u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E21\u0E34\u0E48\u0E07\u0E17\u0E33\u0E2A\u0E34\
  \u0E48\u0E07\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E1B\u0E23\u0E30\u0E2A\
  \u0E34\u0E17\u0E18\u0E34\u0E20\u0E32\u0E1E; \u0E21\u0E31\u0E19\u0E40\u0E23\u0E47\
  \u0E27\u0E01\u0E27\u0E48\u0E32\u0E01\u0E32\u0E23\u0E40\u0E14\u0E34\u0E19\u0E17\u0E32\
  \u0E07\u0E1C\u0E48\u0E32\u0E19 GUI \u0E2B\u0E23\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E40\
  \u0E02\u0E35\u0E22\u0E19\u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E17\u0E35\u0E48\
  \u0E22\u0E32\u0E27\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E07\u0E32\u0E19\u0E07\u0E48\
  \u0E32\u0E22\u0E46."
title: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E44\u0E1F\u0E25\u0E4C\
  \u0E14\u0E49\u0E27\u0E22 CLI \u0E41\u0E1A\u0E1A\u0E43\u0E0A\u0E49\u0E1A\u0E23\u0E23\
  \u0E17\u0E31\u0E14\u0E40\u0E14\u0E35\u0E22\u0E27"
weight: 31
---

## วิธีการ:


### การอ่านไฟล์
เพื่อแสดงเนื้อหาของไฟล์อย่างรวดเร็ว, ใช้คำสั่ง `Get-Content`:
```PowerShell
Get-Content .\example.txt
```

### การเขียนไปยังไฟล์
เพื่อเขียนสิ่งใหม่ไปยังไฟล์, สามารถใช้ `Set-Content`:
```PowerShell
Set-Content -Path .\example.txt -Value "Hello, PowerShell!"
```

### การเพิ่มไปยังไฟล์
การเพิ่มข้อมูลไปยังส่วนท้ายของไฟล์โดยไม่ลบเนื้อหาที่มีอยู่สามารถทำได้ด้วย `Add-Content`:
```PowerShell
Add-Content -Path .\example.txt -Value "Adding this line."
```

### การคัดลอกไฟล์
การคัดลอกไฟล์เป็นเรื่องที่ตรงไปตรงมาด้วย `Copy-Item`:
```PowerShell
Copy-Item -Path .\example.txt -Destination .\copy_of_example.txt
```

### การลบไฟล์
เพื่อลบไฟล์, เพียงใช้ `Remove-Item`:
```PowerShell
Remove-Item -Path .\unwanted_file.txt
```

### การค้นหาภายในไฟล์
ใช้ `Select-String` สำหรับการค้นหาข้อความภายในไฟล์:
```PowerShell
Select-String -Path .\*.txt -Pattern "PowerShell"
```

### การรวมคำสั่ง
PowerShell จริงๆ แล้วเปล่งประกายด้วยความสามารถในการเชื่อมโยงคำสั่งด้วย pipes. นี่คือวิธีที่คุณสามารถค้นหาไฟล์และคัดลอกไปยังไดเร็กทอรี่ใหม่:
```PowerShell
Get-ChildItem -Path .\*.log | Copy-Item -Destination C:\Logs
```

## ศึกษาลึกซึ้ง
ในอดีต, PowerShell ถูกนำมาใช้เป็นทางเลือกที่มีพลังมากขึ้นกว่า command prompt แบบดั้งเดิมใน Windows, โดยนำเสนอการเข้าถึงระบบภายในและคลังข้อมูลที่ไม่มีที่เปรียบ. มันผสานความเร็วของ command-line กับความยืดหยุ่นของสคริปติ้ง, ทำให้เป็นเครื่องมือที่มีค่าไม่ว่าจะสำหรับผู้ดูแลระบบและนักพัฒนาที่ใช้ Windows.

ทางเลือกสำหรับ PowerShell สำหรับการจัดการไฟล์ ได้แก่ เครื่องมือที่ใช้งานบน Unix เช่น `sed`, `awk`, `grep`, และการเขียนสคริปด้วย `bash` สำหรับผู้ใช้ Linux และ MacOS. ขณะที่เครื่องมือเหล่านี้มีความสามารถสูงและมีจุดเด่นของตนเอง, PowerShell นำเสนอการผสานรวมอย่างลึกซึ้งกับสภาพแวดล้อม Windows.

แง่น่าสนใจของ PowerShell คือ ธรรมชาติที่เป็นวัตถุ ไม่เหมือนกับภาษาสคริปต์หลายภาษาที่มองทุกสิ่งเป็นสตริงหรือกระแสของไบต์, PowerShell ทำงานโดยตรงกับวัตถุ .NET. นี่หมายความว่าเมื่อคุณจัดการไฟล์, คุณกำลังทำงานด้วยวัตถุที่มีคุณสมบัติและเมธอดมากมาย, ทำให้งานที่ซับซ้อนเป็นไปได้ง่ายขึ้น.

หนึ่งในจุดอ่อนของ PowerShell, โดยเฉพาะสำหรับผู้ใช้ Linux และ MacOS, คือความยาวเหยียดเมื่อเปรียบเทียบกับการเขียนสคริปท์ bash หรือการใช้เครื่องมือ command-line ของ Unix. นอกจากนี้, การผสานรวมอย่างลึกซึ้งกับ Windows บางครั้งอาจทำให้สคริปต์ข้ามแพลตฟอร์มเป็นเรื่องท้าทายมากขึ้น, แม้ว่าความพยายามกับ PowerShell Core กำลังมุ่งเน้นไปที่การเชื่อมโยงช่องว่างนั้นอย่างมีประสิทธิภาพ.

ไม่ว่าจะมีจุดอ่อนสักเพียงใด, ความแข็งแกร่งของ PowerShell อยู่ที่ความสามารถในการใช้ one-liner ที่ทรงพลัง, สภาพแวดล้อมการเขียนสคริปการบูรณาการ, และการเข้าถึง Windows ecosystem อย่างครอบคลุม, ทำให้เป็นเครื่องมือที่จำเป็นสำหรับผู้ที่ต้องการจัดการไฟล์และอื่นๆ โดยตรงจาก command line.
