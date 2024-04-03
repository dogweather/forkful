---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:28.762294-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E25\u0E2D\u0E07\u0E40\
  \u0E02\u0E35\u0E22\u0E19\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E40\u0E1E\
  \u0E37\u0E48\u0E2D\u0E04\u0E33\u0E19\u0E27\u0E13\u0E1C\u0E25\u0E23\u0E27\u0E21\u0E02\
  \u0E2D\u0E07\u0E2A\u0E2D\u0E07\u0E15\u0E31\u0E27\u0E40\u0E25\u0E02 \u0E07\u0E48\u0E32\
  \u0E22\u0E46 \u0E41\u0E15\u0E48\u0E21\u0E31\u0E19\u0E2A\u0E32\u0E18\u0E34\u0E15\u0E08\
  \u0E38\u0E14\u0E02\u0E2D\u0E07\u0E40\u0E23\u0E32."
lastmod: '2024-03-17T21:57:56.446966-06:00'
model: gpt-4-0125-preview
summary: "\u0E25\u0E2D\u0E07\u0E40\u0E02\u0E35\u0E22\u0E19\u0E1F\u0E31\u0E07\u0E01\
  \u0E4C\u0E0A\u0E31\u0E19\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E04\u0E33\u0E19\u0E27\u0E13\
  \u0E1C\u0E25\u0E23\u0E27\u0E21\u0E02\u0E2D\u0E07\u0E2A\u0E2D\u0E07\u0E15\u0E31\u0E27\
  \u0E40\u0E25\u0E02 \u0E07\u0E48\u0E32\u0E22\u0E46 \u0E41\u0E15\u0E48\u0E21\u0E31\
  \u0E19\u0E2A\u0E32\u0E18\u0E34\u0E15\u0E08\u0E38\u0E14\u0E02\u0E2D\u0E07\u0E40\u0E23\
  \u0E32."
title: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E23\u0E30\u0E40\u0E1A\u0E35\u0E22\u0E1A\
  \u0E42\u0E04\u0E49\u0E14\u0E40\u0E02\u0E49\u0E32\u0E44\u0E1B\u0E43\u0E19\u0E1F\u0E31\
  \u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19"
weight: 18
---

## วิธีการ:
ลองเขียนฟังก์ชันเพื่อคำนวณผลรวมของสองตัวเลข ง่ายๆ แต่มันสาธิตจุดของเรา

```PowerShell
function Add-Numbers {
    param (
        [int]$FirstNum,
        [int]$SecondNum
    )
    return $FirstNum + $SecondNum
}

# เรียกใช้ฟังก์ชันด้วย 5 และ 10
$sum = Add-Numbers -FirstNum 5 -SecondNum 10
Write-Output "ผลรวมคือ $sum"
```

ตัวอย่างผลลัพธ์:

```
ผลรวมคือ 15
```

## การทำความเข้าใจลึกซึ้ง
ฟังก์ชันใน PowerShell, เช่นเดียวกับในภาษาส่วนใหญ่, เป็นข่าวเก่าแก่ ตั้งแต่ยุคของ Fortran เราได้เริ่มจัดระเบียบโค้ด 'เพื่อไม่ให้ต้องสร้างล้อใหม่' ทางเลือก? แน่นอน, สคริปต์หรือ cmdlets แต่พวกมันขาดความเกลี้ยงเกลาและความไวต่อบริบทของฟังก์ชันภายในสคริปต์

การใช้งาน? ฟังก์ชันอาจจะเรียบง่ายเช่นตัวอย่างของเราหรือซับซ้อนพร้อมการจัดการขอบเขต, การป้อนข้อมูลผ่านท่อ (pipeline) และอื่นๆ ลองดูที่ `ฟังก์ชันขั้นสูง` เช่นมีพารามิเตอร์ที่มีลักษณะเฉพาะ เช่น `[Parameter(Mandatory=$true)]` นี่คือรสชาติของความยืดหยุ่นของ PowerShell

## ดูเพิ่มเติม
- [about_Functions_Advanced_Parameters](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7.1)
- [about_Script_Blocks](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_script_blocks?view=powershell-7.1)
