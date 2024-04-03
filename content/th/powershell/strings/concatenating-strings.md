---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:21.523147-06:00
description: "\u0E01\u0E32\u0E23\u0E23\u0E27\u0E21\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\
  \u0E23\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \u0E40\u0E02\u0E49\u0E32\u0E14\u0E49\u0E27\u0E22\u0E01\u0E31\u0E19\u0E40\u0E2B\u0E21\
  \u0E37\u0E2D\u0E19\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E17\u0E33\u0E23\u0E16\u0E44\
  \u0E1F\u0E02\u0E2D\u0E07\u0E04\u0E33\u0E1E\u0E39\u0E14 \u0E40\u0E23\u0E32\u0E17\u0E33\
  \u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E22\
  \u0E47\u0E1A\u0E40\u0E02\u0E49\u0E32\u0E14\u0E49\u0E27\u0E22\u0E01\u0E31\u0E19\u0E04\
  \u0E48\u0E32\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21 \u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E2A\u0E23\u0E49\u0E32\u0E07\u0E27\u0E25\u0E35 \u0E1B\u0E23\u0E30\u0E42\u0E22\u0E04\
  \u2026"
lastmod: '2024-03-17T21:57:56.431356-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E23\u0E27\u0E21\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\
  \u0E23\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \u0E40\u0E02\u0E49\u0E32\u0E14\u0E49\u0E27\u0E22\u0E01\u0E31\u0E19\u0E40\u0E2B\u0E21\
  \u0E37\u0E2D\u0E19\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E17\u0E33\u0E23\u0E16\u0E44\
  \u0E1F\u0E02\u0E2D\u0E07\u0E04\u0E33\u0E1E\u0E39\u0E14 \u0E40\u0E23\u0E32\u0E17\u0E33\
  \u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E22\
  \u0E47\u0E1A\u0E40\u0E02\u0E49\u0E32\u0E14\u0E49\u0E27\u0E22\u0E01\u0E31\u0E19\u0E04\
  \u0E48\u0E32\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21 \u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E2A\u0E23\u0E49\u0E32\u0E07\u0E27\u0E25\u0E35 \u0E1B\u0E23\u0E30\u0E42\u0E22\u0E04\
  \ \u0E2B\u0E23\u0E37\u0E2D\u0E2D\u0E30\u0E44\u0E23\u0E01\u0E47\u0E15\u0E32\u0E21\
  \u0E17\u0E35\u0E48\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E41\u0E22\u0E01\u0E01\u0E31\u0E19\
  \u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E21\u0E32\u0E40\u0E1B\u0E47\u0E19\u0E2B\
  \u0E19\u0E36\u0E48\u0E07\u0E40\u0E14\u0E35\u0E22\u0E27\u0E01\u0E31\u0E19."
title: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 3
---

## วิธีทำ:
มาเริ่มกันเลย:

```PowerShell
# การใช้งาน '+' operator
$greeting = 'Hello, ' + 'World!'
$greeting # แสดงผล: Hello, World!

# ผ่าน string interpolation
$name = 'Jane'
$welcomeMessage = "Hi, $name, nice to meet you!"
$welcomeMessage # แสดงผล: Hi, Jane, nice to meet you!

# ด้วยการใช้ -f operator (format operator)
$city = 'New York'
$visitMessage = 'Welcome to {0}!' -f $city
$visitMessage # แสดงผล: Welcome to New York!

# StringBuilder สำหรับกรณีที่ซับซ้อน (อาจจะเกินความจำเป็นสำหรับงานง่ายๆ)
$textBuilder = New-Object System.Text.StringBuilder
[void]$textBuilder.Append('PowerShell ')
[void]$textBuilder.Append('is ')
[void]$textBuilder.Append('awesome.')
$textBuilder.ToString() # แสดงผล: PowerShell is awesome.
```

## ลงลึก
ในอดีต การต่อสตริงอาจจะมีปัญหาบ้างในภาษาโปรแกรมมิ่งรุ่นเก่า - ลองคิดเหมือนกับการใช้เทปติดประโยคเข้าด้วยกัน ใน PowerShell มันง่ายมาก

มีวิธีการที่ต่างกันในการทำงาน. การใช้ '+' operator นั้นตรงไปตรงมาแต่อาจจะช้าเมื่อมีสตริงจำนวนมาก String interpolation ด้วย "$variable" นั้นสะอาดกว่า และยอดเยี่ยมสำหรับการแทรกตัวแปรเข้าไปในสตริง การใช้ '-f' operator มีประโยชน์ในสถานการณ์การใช้เทมเพลต

เกี่ยวกับประสิทธิภาพ - ถ้าคุณกำลังรวมสตริงยาวๆ เหมือนเรียงความ คุณจะต้องการบางสิ่งที่หนักหน่วงกว่านั้น เข้ามา `StringBuilder` มันไม่ได้ต่อสตริงทันที แต่จะร้อยสตริงเข้าด้วยกันเมื่อถูกเรียก ช่วยประหยัดเวลาและหน่วยความจำสำหรับงานต่อสตริงขนาดใหญ่

## ดูเพิ่มเติม
- [เกี่ยวกับ Join](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_join?view=powershell-7.3)
- [เกี่ยวกับ Automatic Variables](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7.3) (ดู `$OFS`)
- สำหรับข้อมูลเพิ่มเติมเกี่ยวกับการจัดรูปแบบสตริง ตรวจสอบที่ [Composite Formatting](https://docs.microsoft.com/en-us/dotnet/standard/base-types/composite-formatting).
- และถ้าคุณมีความสนใจ เรื่องละเอียดอ่อนของ [StringBuilder](https://docs.microsoft.com/en-us/dotnet/api/system.text.stringbuilder?view=net-6.0).
