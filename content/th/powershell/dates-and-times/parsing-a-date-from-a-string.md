---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:11.455129-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E08\u0E32\u0E01\u0E2A\u0E15\
  \u0E23\u0E34\u0E07 (Parsing a date from a string) \u0E01\u0E47\u0E04\u0E37\u0E2D\
  \u0E01\u0E32\u0E23\u0E23\u0E39\u0E49\u0E08\u0E33\u0E41\u0E25\u0E30\u0E41\u0E1B\u0E25\
  \u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E17\u0E35\u0E48\u0E40\u0E02\u0E35\u0E22\
  \u0E19\u0E2D\u0E22\u0E39\u0E48\u0E43\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E40\u0E1B\u0E47\u0E19\u0E0A\u0E19\u0E34\u0E14\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E17\u0E35\
  \u0E48 PowerShell\u2026"
lastmod: '2024-03-17T21:57:56.450617-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E08\u0E32\u0E01\u0E2A\u0E15\
  \u0E23\u0E34\u0E07 (Parsing a date from a string) \u0E01\u0E47\u0E04\u0E37\u0E2D\
  \u0E01\u0E32\u0E23\u0E23\u0E39\u0E49\u0E08\u0E33\u0E41\u0E25\u0E30\u0E41\u0E1B\u0E25\
  \u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E17\u0E35\u0E48\u0E40\u0E02\u0E35\u0E22\
  \u0E19\u0E2D\u0E22\u0E39\u0E48\u0E43\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E40\u0E1B\u0E47\u0E19\u0E0A\u0E19\u0E34\u0E14\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E17\u0E35\
  \u0E48 PowerShell\u2026"
title: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2D\
  \u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การแยกวิเคราะห์วันที่จากสตริง (Parsing a date from a string) ก็คือการรู้จำและแปลงวันที่ที่เขียนอยู่ในรูปแบบข้อความเป็นชนิดข้อมูลวันที่ที่ PowerShell สามารถเข้าใจและทำงานร่วมกับได้ โปรแกรมเมอร์ทำเช่นนี้เพื่อจัดการ, จัดรูปแบบ, เปรียบเทียบ, หรือคำนวณวันที่, ซึ่งเป็นงานที่พบบ่อยในสคริปต์ที่จัดการกับไฟล์ log, ข้อมูลที่ผู้ใช้ป้อน, หรือการประมวลผลข้อมูล

## วิธีทำ:
PowerShell ทำให้การแยกวิเคราะห์วันที่จากสตริงเป็นไปอย่างง่ายดายด้วยคำสั่ง `Get-Date` และตัวเร่งประเภท `[datetime]`, ซึ่งทำงานได้ดีสำหรับรูปแบบวันที่มาตรฐาน สำหรับสตริงวันที่ที่ซับซ้อนหรือไม่ตามมาตรฐาน, สามารถใช้เมธอด `[datetime]::ParseExact` เพื่อระบุรูปแบบที่ชัดเจน

### การใช้ `Get-Date` และ `[datetime]`:
```powershell
# การแปลงแบบง่ายๆ โดยใช้ Get-Date
$stringDate = "2023-04-01"
$date = Get-Date $stringDate
echo $date
```
**ตัวอย่างผลลัพธ์:**
```
วันเสาร์, เมษายน 1, 2023 12:00:00 น.
```

```powershell
# การใช้ตัวเร่งประเภท [datetime]
$stringDate = "April 1, 2023"
$date = [datetime]$stringDate
echo $date
```
**ตัวอย่างผลลัพธ์:**
```
วันเสาร์, เมษายน 1, 2023 12:00:00 น.
```

### การใช้ `[datetime]::ParseExact` สำหรับรูปแบบที่ไม่มาตรฐาน:
สำหรับรูปแบบที่ไม่ได้รับการรู้จักโดยอัตโนมัติ, คุณสามารถกำหนดรูปแบบที่ชัดเจนเพื่อให้การแยกวิเคราะห์ถูกต้อง
```powershell
$stringDate = "01-04-2023 14:00"
$format = "dd-MM-yyyy HH:mm"
$culture = [Globalization.CultureInfo]::InvariantCulture
$date = [datetime]::ParseExact($stringDate, $format, $culture)
echo $date
```
**ตัวอย่างผลลัพธ์:**
```
วันเสาร์, เมษายน 1, 2023 2:00:00 หลังเที่ยง
```

### การใช้พลังของไลบรารีบุคคลที่สาม
แม้ว่า PowerShell จะมีพลังมากพอสำหรับการแยกวิเคราะห์วันที่, สำหรับสถานการณ์ที่ซับซ้อนมากหรือฟังก์ชันเพิ่มเติม, คุณอาจสำรวจไลบรารี .NET เช่น NodaTime แต่สำหรับกรณีการใช้งานทั่วไปมากมาย, ความสามารถของ PowerShell ดั้งเดิมก็เพียงพอแล้ว

```powershell
# การใช้ NodaTime เพียงเพื่อเป็นการสาธิต, โปรดทราบว่าคุณต้องเพิ่มไลบรารีลงในโปรเจ็กต์ของคุณ
# Install-Package NodaTime -Version 3.0.5
# การใช้ NodaTime เพื่อแยกวิเคราะห์วันที่
[string]$stringDate = "2023-04-01T14:00:00Z"
[NodaTime.Instant]::FromDateTimeUtc([datetime]::UtcNow)
[NodaTime.LocalDate]$localDate = [NodaTime.LocalDate]::FromDateTime([datetime]::UtcNow)
echo $localDate
```
**หมายเหตุตัวอย่าง:** โค้ดด้านบนเป็นเพียงการสาธิตแนวคิด เมื่อใช้งานจริง, ตรวจสอบให้แน่ใจว่าได้เพิ่ม NodaTime ลงในโปรเจ็กต์ของคุณอย่างถูกต้องเพื่อให้ชนิดและเมธอดสามารถใช้งานได้
