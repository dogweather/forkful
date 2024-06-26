---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:21.482086-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: PowerShell \u0E21\u0E35\
  \ cmdlets \u0E17\u0E35\u0E48\u0E15\u0E23\u0E07\u0E44\u0E1B\u0E15\u0E23\u0E07\u0E21\
  \u0E32\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\
  \u0E49\u0E2D\u0E21\u0E39\u0E25\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E41\u0E25\u0E30\
  \u0E40\u0E27\u0E25\u0E32 `Get-Date` cmdlet \u0E04\u0E37\u0E2D\u0E40\u0E04\u0E23\u0E37\
  \u0E48\u0E2D\u0E07\u0E21\u0E37\u0E2D\u0E2B\u0E25\u0E31\u0E01\u0E2A\u0E33\u0E2B\u0E23\
  \u0E31\u0E1A\u0E27\u0E31\u0E15\u0E16\u0E38\u0E1B\u0E23\u0E30\u0E2A\u0E07\u0E04\u0E4C\
  \u0E19\u0E35\u0E49\u2026"
lastmod: '2024-03-17T21:57:56.451578-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u0E21\u0E35 cmdlets \u0E17\u0E35\u0E48\u0E15\u0E23\u0E07\u0E44\
  \u0E1B\u0E15\u0E23\u0E07\u0E21\u0E32\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\
  \u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\u0E25\u0E32 `Get-Date` cmdlet \u0E04\u0E37\
  \u0E2D\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E21\u0E37\u0E2D\u0E2B\u0E25\u0E31\
  \u0E01\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E27\u0E31\u0E15\u0E16\u0E38\u0E1B\u0E23\
  \u0E30\u0E2A\u0E07\u0E04\u0E4C\u0E19\u0E35\u0E49 \u0E21\u0E31\u0E19\u0E2A\u0E32\u0E21\
  \u0E32\u0E23\u0E16\u0E04\u0E37\u0E19\u0E04\u0E48\u0E32\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E41\u0E25\u0E30\u0E40\u0E27\u0E25\u0E32\u0E40\u0E15\u0E47\u0E21\u0E23\u0E39\
  \u0E1B\u0E41\u0E1A\u0E1A\u0E17\u0E35\u0E48\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\
  \u0E23\u0E16\u0E08\u0E31\u0E14\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E2B\u0E23\u0E37\
  \u0E2D\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E15\u0E32\u0E21\u0E04\u0E27\u0E32\u0E21\
  \u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13."
title: "\u0E01\u0E32\u0E23\u0E23\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E1B\
  \u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19"
weight: 29
---

## วิธีการ:
PowerShell มี cmdlets ที่ตรงไปตรงมาสำหรับการดึงข้อมูลวันที่และเวลา `Get-Date` cmdlet คือเครื่องมือหลักสำหรับวัตถุประสงค์นี้ มันสามารถคืนค่าวันที่และเวลาเต็มรูปแบบที่คุณสามารถจัดรูปแบบหรือจัดการตามความต้องการของคุณ

```powershell
# ดึงข้อมูลวันที่และเวลาปัจจุบัน
Get-Date
```

**ผลลัพธ์ตัวอย่าง:**

```
วันอังคาร, 5 กันยายน 2023 9:46:02 น.
```

คุณยังสามารถจัดรูปแบบผลลัพธ์เพื่อแสดงเฉพาะข้อมูลที่คุณต้องการ เช่น เฉพาะวันที่หรือเฉพาะเวลา

```powershell
# ดึงข้อมูลวันที่ปัจจุบันในรูปแบบที่เฉพาะเจาะจง
Get-Date -Format "yyyy-MM-dd"
```

**ผลลัพธ์ตัวอย่าง:**

```
2023-09-05
```

```powershell
# ดึงเฉพาะเวลาปัจจุบัน
Get-Date -Format "HH:mm:ss"
```

**ผลลัพธ์ตัวอย่าง:**

```
09:46:02
```

### การใช้ .NET Class
PowerShell อนุญาตให้เข้าถึงคลาส .NET โดยตรง นำเสนอวิธีอื่นในการทำงานกับวันที่และเวลา

```powershell
# การใช้งาน .NET DateTime class เพื่อดึงข้อมูลวันที่และเวลาปัจจุบัน
[System.DateTime]::Now
```

**ผลลัพธ์ตัวอย่าง:**

```
วันอังคาร, 5 กันยายน 2023 9:46:02 น.
```

สำหรับเวลา UTC:

```powershell
# การใช้งาน .NET DateTime class เพื่อดึงข้อมูลเวลา UTC ปัจจุบัน
[System.DateTime]::UtcNow
```

**ผลลัพธ์ตัวอย่าง:**

```
วันอังคาร, 5 กันยายน 2023 1:46:02 หลังจากเที่ยง
```

คำสั่งและคลาสเหล่านี้ให้ตัวเลือกที่ทรงพลังและยืดหยุ่นสำหรับการทำงานกับวันที่และเวลาใน PowerShell ซึ่งจำเป็นสำหรับงานสคริปต์และอัตโนมัติหลายรูปแบบ
