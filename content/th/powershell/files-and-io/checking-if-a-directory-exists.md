---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:41.661811-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: PowerShell \u0E21\u0E2D\u0E1A\
  \u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E15\u0E23\u0E07\u0E44\u0E1B\u0E15\u0E23\
  \u0E07\u0E21\u0E32\u0E43\u0E19\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\
  \u0E1A\u0E01\u0E32\u0E23\u0E21\u0E35\u0E2D\u0E22\u0E39\u0E48\u0E02\u0E2D\u0E07\u0E44\
  \u0E14\u0E40\u0E23\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\
  \u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07 `Test-Path` \u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\
  \u0E19\u0E35\u0E49\u0E08\u0E30\u0E04\u0E37\u0E19\u0E04\u0E48\u0E32\u0E41\u0E1A\u0E1A\
  \ Boolean\u2026"
lastmod: '2024-03-17T21:57:56.455335-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u0E21\u0E2D\u0E1A\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E15\
  \u0E23\u0E07\u0E44\u0E1B\u0E15\u0E23\u0E07\u0E21\u0E32\u0E43\u0E19\u0E01\u0E32\u0E23\
  \u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E01\u0E32\u0E23\u0E21\u0E35\u0E2D\u0E22\
  \u0E39\u0E48\u0E02\u0E2D\u0E07\u0E44\u0E14\u0E40\u0E23\u0E01\u0E17\u0E2D\u0E23\u0E35\
  \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07 `Test-Path`\
  \ \u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\u0E19\u0E35\u0E49\u0E08\u0E30\u0E04\u0E37\
  \u0E19\u0E04\u0E48\u0E32\u0E41\u0E1A\u0E1A Boolean \u0E0B\u0E36\u0E48\u0E07\u0E1A\
  \u0E48\u0E07\u0E1A\u0E2D\u0E01\u0E27\u0E48\u0E32\u0E21\u0E35\u0E40\u0E2A\u0E49\u0E19\
  \u0E17\u0E32\u0E07\u0E17\u0E35\u0E48\u0E23\u0E30\u0E1A\u0E38\u0E2D\u0E22\u0E39\u0E48\
  \u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E21\u0E48 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\
  \u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\
  \u0E16\u0E43\u0E0A\u0E49\u0E21\u0E31\u0E19\u0E44\u0E14\u0E49."
title: "\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E21\u0E35\u0E44\
  \u0E14\u0E40\u0E23\u0E47\u0E01\u0E17\u0E2D\u0E23\u0E35\u0E2B\u0E23\u0E37\u0E2D\u0E44\
  \u0E21\u0E48"
weight: 20
---

## วิธีการ:
PowerShell มอบวิธีที่ตรงไปตรงมาในการตรวจสอบการมีอยู่ของไดเรกทอรีโดยใช้คำสั่ง `Test-Path` คำสั่งนี้จะคืนค่าแบบ Boolean ซึ่งบ่งบอกว่ามีเส้นทางที่ระบุอยู่หรือไม่ นี่คือวิธีที่คุณสามารถใช้มันได้:

```powershell
# ตรวจสอบว่ามีไดเรกทอรีหรือไม่
$directoryPath = "C:\ExamplePath"
$directoryExists = Test-Path -Path $directoryPath
Write-Output "ไดเรกทอรีมีอยู่หรือไม่? $directoryExists"
```

ตัวอย่างผลลัพธ์สำหรับไดเรกทอรีที่มีอยู่:

```
ไดเรกทอรีมีอยู่หรือไม่? True
```

และสำหรับไดเรกทอรีที่ไม่มีอยู่:

```
ไดเรกทอรีมีอยู่หรือไม่? False
```

สำหรับสคริปต์ที่ซับซ้อนกว่านั้น, โดยเฉพาะอย่างยิ่งที่มีการโต้ตอบกับ network shares หรือ cloud storage, คุณอาจต้องการตรวจสอบเพิ่มเติมหรือฟังก์ชั่นที่ไม่ได้มีให้โดยตรงผ่าน `Test-Path` ในกรณีเช่นนี้, การใช้งานโมดูลของ PowerShell จากบุคคลที่สามหรือไลบรารีอาจเป็นประโยชน์, แม้ว่างานทั่วไปส่วนใหญ่สามารถทำได้ด้วยคำสั่งภายในของ PowerShell เอง ณ อัพเดตความรู้ครั้งล่าสุดของฉัน, ยังไม่มีไลบรารีจากบุคคลที่สามที่ได้รับการยอมรับอย่างกว้างขวางเฉพาะสำหรับการตรวจสอบการมีอยู่ของไดเรกทอรีเกินกว่าที่ `Test-Path` มีให้, เนื่องด้วย `Test-Path` เองมีทั้งความเข้มแข็งและความมีประสิทธิภาพสำหรับวัตถุประสงค์นี้
