---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:55.399025-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E44\u0E1F\u0E25\u0E4C\
  \u0E0A\u0E31\u0E48\u0E27\u0E04\u0E23\u0E32\u0E27\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E44\u0E1F\u0E25\u0E4C\u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\
  \u0E43\u0E19\u0E23\u0E30\u0E22\u0E30\u0E2A\u0E31\u0E49\u0E19 \u0E21\u0E31\u0E01\u0E08\
  \u0E30\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25\u0E23\u0E30\u0E2B\u0E27\u0E48\u0E32\u0E07\u0E40\u0E0B\u0E2A\u0E0A\u0E31\
  \u0E48\u0E19\u2026"
lastmod: '2024-03-17T21:57:56.460050-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E44\u0E1F\u0E25\u0E4C\
  \u0E0A\u0E31\u0E48\u0E27\u0E04\u0E23\u0E32\u0E27\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E44\u0E1F\u0E25\u0E4C\u0E2A\
  \u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\
  \u0E43\u0E19\u0E23\u0E30\u0E22\u0E30\u0E2A\u0E31\u0E49\u0E19 \u0E21\u0E31\u0E01\u0E08\
  \u0E30\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25\u0E23\u0E30\u0E2B\u0E27\u0E48\u0E32\u0E07\u0E40\u0E0B\u0E2A\u0E0A\u0E31\
  \u0E48\u0E19\u2026"
title: "\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E44\u0E1F\u0E25\u0E4C\u0E0A\u0E31\u0E48\u0E27\
  \u0E04\u0E23\u0E32\u0E27"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การสร้างไฟล์ชั่วคราวหมายถึงการสร้างไฟล์สำหรับการใช้งานในระยะสั้น มักจะเพื่อเก็บข้อมูลระหว่างเซสชั่น โปรแกรมเมอร์ทำแบบนี้เพื่อหลีกเลี่ยงการทำให้ระบบรกและดูแลข้อมูลที่ไม่จำเป็นต้องมีอยู่แบบถาวร

## วิธีทำ:
เพื่อสร้างไฟล์ชั่วคราวใน PowerShell คุณใช้ `New-TemporaryFile` คำสั่งนี้สร้างไฟล์ชั่วคราวในโฟลเดอร์ temp ของคุณ นี่คือคาถาวิเศษ:

```PowerShell
$tempFile = New-TemporaryFile
```

บรรทัดนี้เรียกไฟล์ชั่วคราวใหม่ออกมาจากอากาศดิจิทัล อยากทราบว่ามันอยู่ที่ไหน? เพียงพิมพ์:

```PowerShell
$tempFile.FullName
```

และตุบ! มันจะบอกคุณเส้นทางของไฟล์ เมื่อคุณทำเสร็จและต้องการทำความสะอาด แค่ลบมันออก:

```PowerShell
Remove-Item $tempFile.FullName
```

ไฟล์จะหายไปโดยไม่เหลือร่องรอย

## ศึกษารายละเอียด
ตอนนี้ มาดูภายใต้ประทุนกัน เดิมที ไฟล์ชั่วคราวถูกใช้ตั้งแต่ยุคเริ่มแรกของการคอมพิวต์ เนื่องจาก RAM เป็นสิ่งที่หายากและมีราคาแพง ไฟล์ชั่วควายเหล่านี้เป็นวิธีแก้ไขปัญหาสำหรับหน่วยความจำที่จำกัด

เมื่อพูดถึงทางเลือก บางคนนักพัฒนาเลือกที่จะสร้างเส้นทางไฟล์ชั่วคราวของตนเองโดยใช้ `[System.IO.Path]::GetTempFileName()` ซึ่งทำงานได้ในหลายภาษาที่รองรับ .NET และให้ความยืดหยุ่นมากขึ้น

ใน PowerShell, `New-TemporaryFile` จริงๆ แล้วเป็นตัวห่อที่ดูสะอาดสวยรอบรอบวิธีการ .NET นี้ มันสร้างไฟล์ที่เส้นทางเช่น `C:\Users\YourName\AppData\Local\Temp\tmpXXXX.tmp` (`XXXX` เป็นตัวเลขสุ่ม) นามสกุล `.tmp` เป็นการแสดงนิสัยชั่วคราว

จำไว้ว่า ไฟล์ชั่วคราวควรถูกกำจัดอย่างถูกต้อง หากคุณกำลังสร้างจำนวนมากหรือจัดการกับข้อมูลที่ละเอียดอ่อน คุณควรทำการลบข้อมูลอย่างปลอดภัยเพื่อป้องกันการรั่วไหลของข้อมูล

## ดูเพิ่มเติม
- สำหรับข้อมูลเพิ่มเติมเกี่ยวกับ `New-TemporaryFile` ตรวจสอบที่[เอกสาร](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/new-temporaryfile)
- ดูวิธีการของคลาส `System.IO.Path` บน [Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.io.path?view=net-6.0)
