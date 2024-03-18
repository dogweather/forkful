---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:56.296966-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E43\u0E19 PowerShell\
  \ \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E25\
  \u0E35\u0E48\u0E22\u0E19\u0E27\u0E31\u0E15\u0E16\u0E38 `DateTime` \u0E40\u0E1B\u0E47\
  \u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\
  \ \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\
  \u0E33\u0E01\u0E32\u0E23\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E08\u0E31\
  \u0E14\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E2A\u0E14\u0E07\u0E1C\u0E25\
  , \u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01,\u2026"
lastmod: '2024-03-17T21:57:56.452543-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E43\u0E19 PowerShell\
  \ \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E25\
  \u0E35\u0E48\u0E22\u0E19\u0E27\u0E31\u0E15\u0E16\u0E38 `DateTime` \u0E40\u0E1B\u0E47\
  \u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\
  \ \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\
  \u0E33\u0E01\u0E32\u0E23\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E08\u0E31\
  \u0E14\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E41\u0E2A\u0E14\u0E07\u0E1C\u0E25\
  , \u0E1A\u0E31\u0E19\u0E17\u0E36\u0E01,\u2026"
title: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

การแปลงวันที่เป็นสตริงใน PowerShell หมายถึงการเปลี่ยนวัตถุ `DateTime` เป็นรูปแบบข้อความ โปรแกรมเมอร์ทำการนี้เพื่อจัดรูปแบบวันที่สำหรับการแสดงผล, บันทึก, ชื่อไฟล์, หรือเพื่อการซีเรียลไลซ์ข้อมูลเพื่อการเก็บเข้าสู่คลังและการถ่ายโอนข้อมูล

## วิธีการ:

เพื่อทำการแปลงวันที่เป็นสตริง เราใช้เมธอด `ToString` หรือตัวดำเนินการรูปแบบ `-f` นี่คือวิธีการ:

```PowerShell
# วันที่และเวลาปัจจุบัน
$date = Get-Date

# การแปลงเป็นสตริงแบบค่าเริ่มต้น
$dateString = $date.ToString()
Write-Output $dateString

# รูปแบบที่กำหนดเอง: ปี-เดือน-วัน ชั่วโมง:นาที
$customFormat = $date.ToString("yyyy-MM-dd HH:mm")
Write-Output $customFormat

# การใช้ตัวดำเนินการ -f สำหรับรูปแบบที่กำหนดเองเดียวกัน
$fString = "{0:yyyy-MM-dd HH:mm}" -f $date
Write-Output $fString
```

ตัวอย่างผลลัพธ์:

```
2023-03-17 10:45:00
2023-03-17 10:45
2023-03-17 10:45
```

## ดำดิ่งลึก

PowerShell, ซึ่งได้แรงบันดาลใจจาก shell ของ Unix และ Windows Script Host, เปิดตัวคำสั่ง `Get-Date` ในช่วงพัฒนาการตอนต้นราวปี 2006 ที่กลายเป็นคำสั่งหลักสำหรับการดำเนินการเกี่ยวกับวันที่และเวลา เมธอด `ToString` บนวัตถุ `DateTime` และตัวดำเนินการรูปแบบ `-f` เป็นแนวคิดที่ยืมมาจาก .NET ให้ PowerShell มีลักษณะแบบวัตถุ

หากไม่ระบุรูปแบบลงใน `ToString()` จะแสดงวันที่และเวลาเต็มในรูปแบบปัจจุบันของวัฒนธรรม แต่เมื่อคุณต้องการรูปแบบเฉพาะเช่น ISO 8601 หรือเพียงแค่วันและเดือน สตริงรูปแบบวันที่และเวลาของ .NET กลายเป็นเพื่อนรักของคุณ

มีวิธีแบบเก่าอีกอย่างในการใช้รูปแบบของ `DateTime` เช่น `yyyy` สำหรับปีสี่หลัก, `MM` สำหรับเดือนที่เติมศูนย์ พวกมันเป็นสัญชาตญาณและมีให้เลือกมากมายสำหรับการสร้างรูปแบบวันที่และเวลา

จากนั้นมี POSIX ใน Unix ที่คำสั่ง `date` มีบทบาทพร้อมตัวกำหนดรูปแบบของตนเอง PowerShell เชื่อมโยงสองโลกเข้าด้วยกัน โดยยึดแนวทางที่คุ้นเคยแต่ยังให้ความเข้ากันได้อย่างหนักกับระบบ Windows

ทางเลือกอื่น ๆ รวมถึงการใช้การต่อสตริงของส่วนประกอบวันที่และการใช้ยูทิลิตี้ภายนอกหรือโครงสร้างภาษา PowerShell อย่างไรก็ตาม ต้องการให้เรื่องราวมีความเข้มแข็งด้วยคำสั่งในบ้านเป็นหลัก

คุณสามารถศึกษารายละเอียดเพิ่มเติมเกี่ยวกับตัวกำหนดรูปแบบในเอกสารทางการของ Microsoft หรือค้นหาบล็อกที่เขียนโดยชุมชนซึ่งมักแบ่งปันวิธีการสร้างสรรค์ในการจัดการวันที่และเวลาใน PowerShell

## ดูเพิ่มเติม

- เอกสารการใช้งานทางการของ PowerShell เกี่ยวกับคำสั่ง [Get-Date](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/get-date) ซึ่งให้ข้อมูลการใช้งานและตัวอย่าง
- คู่มือสตริงรูปแบบมาตรฐานและกำหนดเองของ .NET [ซึ่งให้รายละเอียดการจัดรูปแบบอย่างลึกซึ้ง](https://docs.microsoft.com/dotnet/standard/base-types/standard-date-and-time-format-strings)
- บล็อกของชุมชนเช่น [ฟอรั่ม PowerShell.org](https://powershell.org/forums/) หรือ [Stack Overflow](https://stackoverflow.com/questions/tagged/powershell+datetime) สำหรับตัวอย่างจากโลกแห่งความเป็นจริงและการอภิปรายปัญหา
