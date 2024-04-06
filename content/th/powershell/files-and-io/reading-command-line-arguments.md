---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:45.359700-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23 PowerShell \u0E2D\u0E48\u0E32\
  \u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E1A\
  \u0E23\u0E23\u0E17\u0E31\u0E14\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\u0E42\u0E14\u0E22\
  \u0E43\u0E0A\u0E49\u0E2D\u0E32\u0E23\u0E4C\u0E40\u0E23\u0E22\u0E4C `$args` \u0E2B\
  \u0E23\u0E37\u0E2D\u0E1E\u0E32\u0E23\u0E32\u0E21\u0E34\u0E40\u0E15\u0E2D\u0E23\u0E4C\
  \ `$args` \u0E40\u0E2B\u0E21\u0E32\u0E30\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E2A\
  \u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E17\u0E35\u0E48\u0E43\u0E0A\u0E49\u0E04\u0E23\
  \u0E31\u0E49\u0E07\u0E40\u0E14\u0E35\u0E22\u0E27;\u2026"
lastmod: '2024-04-05T22:39:01.390919-06:00'
model: gpt-4-0125-preview
summary: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23 PowerShell \u0E2D\u0E48\u0E32\
  \u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\u0E34\u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E1A\
  \u0E23\u0E23\u0E17\u0E31\u0E14\u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\u0E42\u0E14\u0E22\
  \u0E43\u0E0A\u0E49\u0E2D\u0E32\u0E23\u0E4C\u0E40\u0E23\u0E22\u0E4C `$args` \u0E2B\
  \u0E23\u0E37\u0E2D\u0E1E\u0E32\u0E23\u0E32\u0E21\u0E34\u0E40\u0E15\u0E2D\u0E23\u0E4C\
  \ `$args` \u0E40\u0E2B\u0E21\u0E32\u0E30\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E2A\
  \u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E17\u0E35\u0E48\u0E43\u0E0A\u0E49\u0E04\u0E23\
  \u0E31\u0E49\u0E07\u0E40\u0E14\u0E35\u0E22\u0E27; \u0E1E\u0E32\u0E23\u0E32\u0E21\
  \u0E34\u0E40\u0E15\u0E2D\u0E23\u0E4C\u0E14\u0E35\u0E01\u0E27\u0E48\u0E32\u0E2A\u0E33\
  \u0E2B\u0E23\u0E31\u0E1A\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E21\u0E37\u0E2D\
  \u0E17\u0E35\u0E48\u0E41\u0E02\u0E47\u0E07\u0E41\u0E01\u0E23\u0E48\u0E07."
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E2D\u0E32\u0E23\u0E4C\u0E01\u0E34\
  \u0E27\u0E40\u0E21\u0E19\u0E15\u0E4C\u0E08\u0E32\u0E01\u0E04\u0E33\u0E2A\u0E31\u0E48\
  \u0E07\u0E25\u0E33\u0E14\u0E31\u0E1A"
weight: 23
---

## วิธีการ
PowerShell อ่านอาร์กิวเมนต์บรรทัดคำสั่งโดยใช้อาร์เรย์ `$args` หรือพารามิเตอร์ `$args` เหมาะสำหรับสคริปต์ที่ใช้ครั้งเดียว; พารามิเตอร์ดีกว่าสำหรับเครื่องมือที่แข็งแกร่ง

### การใช้ `$args`
```PowerShell
# myscript.ps1
Write-Host "คุณได้ป้อนอาร์กิวเมนต์ต่อไปนี้:"
$args
```
เรียกใช้ด้วย `.\myscript.ps1 Hello PowerShell`, แสดงผล:
```
คุณได้ป้อนอาร์กิวเมนต์ต่อไปนี้:
Hello PowerShell
```

### การใช้พารามิเตอร์
```PowerShell
# myscriptparam.ps1
param (
    [string]$Name,
    [int]$Age
)
Write-Host "สวัสดี, $Name! คุณอายุ $Age ปี."
```
เรียกใช้ด้วย `.\myscriptparam.ps1 -Name Sarah -Age 32`, แสดงผล:
```
สวัสดี, Sarah! คุณอายุ 32 ปี.
```

## ศึกษาเพิ่มเติม
วิธีการทันสมัยของ PowerShell ในการอ่านอาร์กิวเมนต์บรรทัดคำสั่งคล้ายกับสิ่งที่ได้รับมาจากบรรพบุรุษเช่น cmd และ Bash อย่างไรก็ตาม, มันขยายความยืดหยุ่นและความแม่นยำ

### บริบททางประวัติศาสตร์
หลายปีก่อน, ไฟล์แบทช์และสคริปต์เชลล์เข้าถึงอาร์กิวเมนต์โดยใช้ตัวแปรที่มีตัวเลข (เช่น `%1`, `%2`) PowerShell ได้ปรับปรุงสิ่งนี้ด้วย `$args` และพารามิเตอร์ที่มีชื่อเพื่อความชัดเจนและควบคุมมากขึ้น

### ทางเลือก
มีทางเลือกอื่นๆ เช่นการแยกวิเคราะห์อินพุตดิบด้วย `Read-Host` หรือการรับอินพุตที่ถูกส่งผ่านไป (`piped input`) อย่างไรก็ตาม, `$args` และพารามิเตอร์นั้นง่ายกว่ามากสำหรับงานอัตโนมัติและสคริปต์

### รายละเอียดการใช้งาน
`$args` เป็นอาร์เรย์ง่ายๆ ดีสำหรับอินพุตที่ไม่เจาะจง พารามิเตอร์ ด้วยคุณลักษณะและประเภทของมันสามารถตรวจสอบอินพุตและแม้กระทั่งส่งคำเชิญให้ผู้ใช้ ทำให้สคริปต์มีการเอกสารอย่างชัดเจนและลดโอกาสข้อผิดพลาดได้

## ดูเพิ่มเติมได้ที่
- [เกี่ยวกับพารามิเตอร์](https://docs.microsoft.com/en-us/powershell/scripting/developer/cmdlet/cmdlet-parameter-sets?view=powershell-7)
- [ตัวแปรอัตโนมัติใน PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_automatic_variables?view=powershell-7&viewFallbackFrom=powershell-6)
