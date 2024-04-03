---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:18.343810-06:00
description: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\
  \u0E40\u0E27\u0E47\u0E1A\u0E40\u0E1E\u0E08\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\
  \u0E01\u0E32\u0E23\u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E40\
  \u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E1C\u0E48\u0E32\u0E19\u0E17\u0E32\u0E07\u0E40\
  \u0E27\u0E47\u0E1A \u0E19\u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E17\u0E33\u0E40\
  \u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\
  \u0E40\u0E27\u0E47\u0E1A\u0E2A\u0E01\u0E23\u0E35\u0E1B, \u0E01\u0E32\u0E23\u0E14\
  \u0E39\u0E41\u0E1A\u0E1A\u0E2D\u0E2D\u0E1F\u0E44\u0E25\u0E19\u0E4C \u0E2B\u0E23\u0E37\
  \u0E2D\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E2D\u0E31\u0E15\u0E42\
  \u0E19\u0E21\u0E31\u0E15\u0E34\u0E01\u0E32\u0E23\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\
  \u0E01\u0E31\u0E1A\u0E40\u0E27\u0E47\u0E1A\u0E44\u0E0B\u0E15\u0E4C"
lastmod: '2024-03-17T21:57:56.440237-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\
  \u0E40\u0E27\u0E47\u0E1A\u0E40\u0E1E\u0E08\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\
  \u0E01\u0E32\u0E23\u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E40\
  \u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32\u0E1C\u0E48\u0E32\u0E19\u0E17\u0E32\u0E07\u0E40\
  \u0E27\u0E47\u0E1A \u0E19\u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E17\u0E33\u0E40\
  \u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\
  \u0E40\u0E27\u0E47\u0E1A\u0E2A\u0E01\u0E23\u0E35\u0E1B, \u0E01\u0E32\u0E23\u0E14\
  \u0E39\u0E41\u0E1A\u0E1A\u0E2D\u0E2D\u0E1F\u0E44\u0E25\u0E19\u0E4C \u0E2B\u0E23\u0E37\
  \u0E2D\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E2D\u0E31\u0E15\u0E42\
  \u0E19\u0E21\u0E31\u0E15\u0E34\u0E01\u0E32\u0E23\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\
  \u0E01\u0E31\u0E1A\u0E40\u0E27\u0E47\u0E1A\u0E44\u0E0B\u0E15\u0E4C."
title: "\u0E01\u0E32\u0E23\u0E14\u0E32\u0E27\u0E19\u0E4C\u0E42\u0E2B\u0E25\u0E14\u0E2B\
  \u0E19\u0E49\u0E32\u0E40\u0E27\u0E47\u0E1A"
weight: 42
---

## วิธีการ:
นี่คือคาถาวิเศษสำหรับการเรียกดูเว็บเพจโดยใช้ PowerShell เราจะใช้ `Invoke-WebRequest`

```PowerShell
# จับเนื้อหาของ example.com
$response = Invoke-WebRequest -Uri "http://example.com"

# นี่คือสิ่งที่คุณได้รับ
$response.Content
```

ตัวอย่างผลลัพธ์:

```PowerShell
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
    <!-- และอื่นๆ -->
</head>
...
</html>
```

คุณอาจต้องการแค่ข้อความ, ไม่มีแท็ก HTML เราทำแบบนั้นได้ดังนี้:

```PowerShell
# ขอแค่ข้อความเท่านั้น
$response.ParsedHtml.body.innerText
```

## ภาพรวม
เมื่อก่อน PowerShell ไม่มี cmdlet `Invoke-WebRequest` ที่ดีเยี่ยมนี้ นักพัฒนาจะใช้คลาส .NET `System.Net.WebClient` หรือหันไปใช้เครื่องมือภายนอก ตอนนี้, ทุกอย่างถูกฝังมาใน PowerShell แล้ว, ทำให้งานของเราง่ายขึ้น

`Invoke-WebRequest` เสนอมากกว่าแค่เนื้อหา หัวข้อ, สถานะ, และข้อมูลเซสชัน – ทุกอย่างมีหมด ถ้าคุณกำลังเล่นกับ API, คุณจะรัก `Invoke-RestMethod` เป็นทางเลือกที่มุ่งเน้น

ภายใต้ประทุน, cmdlet เหล่านี้พึ่งพาคลาส .NET HttpClient ที่มีน้ำหนัก, ให้ความน่าเชื่อถือและฟังก์ชันการทำงานกว้างขวาง

และถ้าคุณกำลังเร่งรีบรอการดาวน์โหลดเว็บเพจนั้น `Invoke-WebRequest` ยังรองรับการทำงานแบบอะซิงโครนัสด้วย อย่างไรก็ตาม, นั่นเป็นหัวข้อสำหรับอีกวันหนึ่ง

## ดูเพิ่มเติม
- [เอกสารของ Invoke-WebRequest](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest)
- ข้อมูลเพิ่มเติมเกี่ยวกับ [Invoke-RestMethod สำหรับการโต้ตอบกับ API](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-restmethod)
- [ที่เก็บ GitHub ของ PowerShell](https://github.com/PowerShell/PowerShell) สำหรับนักพัฒนาที่รักการสำรวจใต้ฝา
