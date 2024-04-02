---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:54:12.416345-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E17\u0E14\u0E2A\u0E2D\
  \u0E1A\u0E43\u0E19 PowerShell \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E17\
  \u0E35\u0E48\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E1F\u0E31\u0E07\u0E01\u0E4C\
  \u0E0A\u0E31\u0E19\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E02\u0E2D\u0E07\
  \u0E42\u0E04\u0E49\u0E14 PowerShell \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E42\u0E14\
  \u0E22\u0E2D\u0E31\u0E15\u0E42\u0E19\u0E21\u0E31\u0E15\u0E34\u2026"
lastmod: '2024-03-17T21:57:56.445108-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E17\u0E14\u0E2A\u0E2D\
  \u0E1A\u0E43\u0E19 PowerShell \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E2A\u0E04\u0E23\u0E34\u0E1B\u0E15\u0E4C\u0E17\
  \u0E35\u0E48\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E1F\u0E31\u0E07\u0E01\u0E4C\
  \u0E0A\u0E31\u0E19\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E02\u0E2D\u0E07\
  \u0E42\u0E04\u0E49\u0E14 PowerShell \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E42\u0E14\
  \u0E22\u0E2D\u0E31\u0E15\u0E42\u0E19\u0E21\u0E31\u0E15\u0E34\u2026"
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E01\u0E32\u0E23\u0E17\u0E14\
  \u0E2A\u0E2D\u0E1A"
weight: 36
---

## อะไรและทำไม?

การเขียนทดสอบใน PowerShell หมายถึงการสร้างสคริปต์ที่ตรวจสอบฟังก์ชันการทำงานของโค้ด PowerShell ของคุณโดยอัตโนมัติ เพื่อให้แน่ใจว่าทุกอย่างทำงานตามที่คาดหวังไว้ โปรแกรมเมอร์ทำสิ่งนี้เพื่อจับบั๊กให้เร็วที่สุด ทำให้การบำรุงรักษาโค้ดง่ายขึ้น และคอยตรวจสอบไม่ให้การเปลี่ยนแปลงโค้ดทำลายฟังก์ชันการทำงานที่มีอยู่โดยไม่ได้ตั้งใจ

## วิธีทำ:

PowerShell ไม่มีเฟรมเวิร์กทดสอบในตัว แต่ Pester ซึ่งเป็นโมดูลจากบุคคลที่สามที่ได้รับความนิยมอย่างกว้างขวาง ถูกใช้สำหรับการเขียนและการทำงานของการทดสอบ นี่คือวิธีเริ่มต้นใช้งาน Pester สำหรับการทดสอบฟังก์ชัน PowerShell ของคุณ

ก่อนอื่น ติดตั้ง Pester หากคุณยังไม่ได้ติดตั้ง:

```powershell
Install-Module -Name Pester -Scope CurrentUser -Force
```

จากนั้น พิจารณาว่าคุณมีฟังก์ชัน PowerShell ที่ง่ายที่คุณต้องการทดสอบ ซึ่งบันทึกไว้เป็น `MyFunction.ps1`:

```powershell
function Get-MultipliedNumber {
    param (
        [int]$Number,
        [int]$Multiplier = 2
    )

    return $Number * $Multiplier
}
```

เพื่อทดสอบฟังก์ชันนี้ด้วย Pester ให้สร้างสคริปต์ทดสอบชื่อว่า `MyFunction.Tests.ps1` ในสคริปต์นี้ ใช้บล็อก `Describe` และ `It` ของ Pester เพื่อกำหนดกรณีทดสอบ:

```powershell
# นำเข้าฟังก์ชันที่จะทดสอบ
. .\MyFunction.ps1

Describe "ทดสอบ Get-MultipliedNumber" {
    It "คูณเลขด้วย 2 เมื่อไม่ได้ให้ตัวคูณ" {
        $result = Get-MultipliedNumber -Number 3
        $result | Should -Be 6
    }

    It "คูณเลขด้วยตัวคูณที่ได้รับอย่างถูกต้อง" {
        $result = Get-MultipliedNumber -Number 3 -Multiplier 3
        $result | Should -Be 9
    }
}
```

เพื่อทำการทดสอบ ให้เปิด PowerShell นำทางไปยังไดเรกทอรี่ที่มีสคริปต์ทดสอบของคุณ และใช้คำสั่ง `Invoke-Pester`:

```powershell
Invoke-Pester .\MyFunction.Tests.ps1
```

ผลลัพธ์ตัวอย่างจะแสดงออกมาเช่นนี้ ชี้ว่าการทดสอบของคุณผ่านหรือไม่ผ่าน:

```
เริ่มต้นการค้นหาใน 1 ไฟล์
การค้นหาเสร็จสิ้นใน 152ms
[+] C:\path\to\MyFunction.Tests.ps1 204ms (182ms|16ms)
การทดสอบเสร็จสิ้นใน 204ms
การทดสอบผ่าน: 2, ไม่ผ่าน: 0, ข้าม: 0 ไม่ได้ทำ: 0
```

ผลลัพธ์นี้แสดงว่าทั้งสองการทดสอบผ่าน ให้ความมั่นใจว่าฟังก์ชัน `Get-MultipliedNumber` ของคุณทำงานตามที่คาดหวังในสถานการณ์ที่คุณทดสอบ
