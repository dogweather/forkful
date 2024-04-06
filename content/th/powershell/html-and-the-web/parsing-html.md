---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:33.496505-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: PowerShell \u0E44\u0E21\u0E48\
  \u0E21\u0E35\u0E15\u0E31\u0E27\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C HTML \u0E42\u0E14\u0E22\u0E15\u0E23\u0E07, \u0E41\u0E15\u0E48\
  \u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\u0E04\u0E33\
  \u0E2A\u0E31\u0E48\u0E07 `Invoke-WebRequest` \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\
  \u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u0E41\u0E25\u0E30\u0E27\u0E34\u0E40\u0E04\u0E23\
  \u0E32\u0E30\u0E2B\u0E4C\u0E40\u0E19\u0E37\u0E49\u0E2D\u0E2B\u0E32 HTML\u2026"
lastmod: '2024-04-05T21:54:02.245509-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u0E44\u0E21\u0E48\u0E21\u0E35\u0E15\u0E31\u0E27\u0E41\u0E22\u0E01\
  \u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C HTML \u0E42\u0E14\u0E22\u0E15\
  \u0E23\u0E07, \u0E41\u0E15\u0E48\u0E04\u0E38\u0E13\u0E2A\u0E32\u0E21\u0E32\u0E23\
  \u0E16\u0E43\u0E0A\u0E49\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07 `Invoke-WebRequest`\
  \ \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E02\u0E49\u0E32\u0E16\u0E36\u0E07\u0E41\
  \u0E25\u0E30\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E40\u0E19\u0E37\
  \u0E49\u0E2D\u0E2B\u0E32 HTML \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\
  \u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E41\u0E25\
  \u0E30\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E17\u0E35\u0E48\u0E0B\
  \u0E31\u0E1A\u0E0B\u0E49\u0E2D\u0E19\u0E22\u0E34\u0E48\u0E07\u0E02\u0E36\u0E49\u0E19\
  , \u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49 HtmlAgilityPack, \u0E44\
  \u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35 .NET \u0E17\u0E35\u0E48\u0E44\u0E14\u0E49\u0E23\
  \u0E31\u0E1A\u0E04\u0E27\u0E32\u0E21\u0E19\u0E34\u0E22\u0E21."
title: "\u0E01\u0E32\u0E23\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C HTML"
weight: 43
---

## วิธีการ:
PowerShell ไม่มีตัวแยกวิเคราะห์ HTML โดยตรง, แต่คุณสามารถใช้คำสั่ง `Invoke-WebRequest` เพื่อเข้าถึงและวิเคราะห์เนื้อหา HTML สำหรับการแยกวิเคราะห์และการจัดการที่ซับซ้อนยิ่งขึ้น, สามารถใช้ HtmlAgilityPack, ไลบรารี .NET ที่ได้รับความนิยม

### การใช้ `Invoke-WebRequest`:
```powershell
# ตัวอย่างง่ายๆในการดึงชื่อหัวข้อจากเว็บ
$response = Invoke-WebRequest -Uri 'http://example.com'
# ใช้สิทธิ์ ParsedHtml เพื่อเข้าถึงองค์ประกอบ DOM
$title = $response.ParsedHtml.title
Write-Output $title
```

ตัวอย่างผลลัพธ์:

```
Example Domain
```

### การใช้ HtmlAgilityPack:
ขั้นแรก, คุณจำเป็นต้องติดตั้ง HtmlAgilityPack คุณสามารถทำได้ผ่าน NuGet Package Manager:

```powershell
Install-Package HtmlAgilityPack -ProviderName NuGet
```

จากนั้น, คุณสามารถใช้มันใน PowerShell เพื่อวิเคราะห์ HTML:

```powershell
# โหลดการรวม HtmlAgilityPack
Add-Type -Path "path\to\HtmlAgilityPack.dll"

# สร้างอ็อบเจกต์ HtmlDocument
$doc = New-Object HtmlAgilityPack.HtmlDocument

# โหลด HTML จากไฟล์หรือคำขอเว็บ
$htmlContent = (Invoke-WebRequest -Uri "http://example.com").Content
$doc.LoadHtml($htmlContent)

# ใช้ XPath หรือวิธีการคิวรีอื่นๆ เพื่อดึงองค์ประกอบ
$node = $doc.DocumentNode.SelectSingleNode("//h1")

if ($node -ne $null) {
    Write-Output $node.InnerText
}
```

ตัวอย่างผลลัพธ์:

```
Welcome to Example.com!
```

ในตัวอย่างเหล่านี้, `Invoke-WebRequest` เหมาะสำหรับงานง่ายๆ, ขณะที่ HtmlAgilityPack นำเสนอชุดคุณสมบัติที่ครอบคลุมมากขึ้นสำหรับการวิเคราะห์ HTML และการจัดการที่ซับซ้อน.
