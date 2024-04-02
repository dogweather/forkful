---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:18.767732-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E15\u0E31\u0E27\u0E40\
  \u0E25\u0E02\u0E2A\u0E38\u0E48\u0E21\u0E43\u0E19 PowerShell \u0E04\u0E37\u0E2D\u0E01\
  \u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E04\u0E48\u0E32\u0E15\u0E31\u0E27\u0E40\
  \u0E25\u0E02\u0E17\u0E35\u0E48\u0E44\u0E21\u0E48\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\
  \u0E17\u0E32\u0E22\u0E44\u0E14\u0E49\u0E20\u0E32\u0E22\u0E43\u0E19\u0E0A\u0E48\u0E27\
  \u0E07\u0E17\u0E35\u0E48\u0E01\u0E33\u0E2B\u0E19\u0E14 \u0E19\u0E31\u0E01\u0E1E\u0E31\
  \u0E12\u0E19\u0E32\u0E43\u0E0A\u0E49\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E32\u0E21\u0E32\
  \u0E23\u0E16\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E2B\u0E25\u0E32\u0E22\
  \u0E40\u0E2B\u0E15\u0E38\u0E1C\u0E25 \u0E23\u0E27\u0E21\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E17\u0E14\u0E2A\u0E2D\u0E1A, \u0E01\u0E32\u0E23\u0E08\u0E33\u0E25\u0E2D\
  \u0E07,\u2026"
lastmod: '2024-03-17T21:57:56.435230-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E15\u0E31\u0E27\u0E40\
  \u0E25\u0E02\u0E2A\u0E38\u0E48\u0E21\u0E43\u0E19 PowerShell \u0E04\u0E37\u0E2D\u0E01\
  \u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E04\u0E48\u0E32\u0E15\u0E31\u0E27\u0E40\
  \u0E25\u0E02\u0E17\u0E35\u0E48\u0E44\u0E21\u0E48\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\
  \u0E17\u0E32\u0E22\u0E44\u0E14\u0E49\u0E20\u0E32\u0E22\u0E43\u0E19\u0E0A\u0E48\u0E27\
  \u0E07\u0E17\u0E35\u0E48\u0E01\u0E33\u0E2B\u0E19\u0E14 \u0E19\u0E31\u0E01\u0E1E\u0E31\
  \u0E12\u0E19\u0E32\u0E43\u0E0A\u0E49\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E32\u0E21\u0E32\
  \u0E23\u0E16\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E2B\u0E25\u0E32\u0E22\
  \u0E40\u0E2B\u0E15\u0E38\u0E1C\u0E25 \u0E23\u0E27\u0E21\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E17\u0E14\u0E2A\u0E2D\u0E1A, \u0E01\u0E32\u0E23\u0E08\u0E33\u0E25\u0E2D\
  \u0E07,\u2026"
title: "\u0E01\u0E32\u0E23\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E15\u0E31\u0E27\u0E40\u0E25\
  \u0E02\u0E2A\u0E38\u0E48\u0E21"
weight: 12
---

## อะไรและทำไม?
การสร้างตัวเลขสุ่มใน PowerShell คือการสร้างค่าตัวเลขที่ไม่สามารถทายได้ภายในช่วงที่กำหนด นักพัฒนาใช้ความสามารถนี้เพื่อหลายเหตุผล รวมถึงการทดสอบ, การจำลอง, และจุดประสงค์ด้านความปลอดภัย ที่ความไม่สามารถทายล่วงหน้าหรือการจำลองความเป็นสุ่มในโลกแห่งความจริงเป็นสิ่งสำคัญ

## วิธีการ:
PowerShell มีวิธีการในการสร้างตัวเลขสุ่มที่ตรงไปตรงมาโดยใช้คำสั่ง `Get-Random` cmdlet คำสั่งนี้สามารถผลิตตัวเลขสุ่มภายในช่วงที่กำหนดได้หรือภายในช่วงที่ระบุ

```PowerShell
# สร้างตัวเลขสุ่มระหว่าง 0 และ Int32.MaxValue
$randomNumber = Get-Random
Write-Output $randomNumber
```

เพื่อระบุช่วง, ใช้พารามิเตอร์ `-Minimum` และ `-Maximum`:

```PowerShell
# สร้างตัวเลขสุ่มระหว่าง 1 และ 100
$randomNumber = Get-Random -Minimum 1 -Maximum 101
Write-Output $randomNumber
```

สำหรับการควบคุมมากขึ้น, คุณสามารถสร้างวัตถุของคลาส `System.Random`:

```PowerShell
# ใช้ System.Random สำหรับลำดับของตัวเลข
$rand = New-Object System.Random
foreach ($i in 1..5) {
    $randomNumber = $rand.Next(1, 101)
    Write-Output $randomNumber
}
```

หากคุณต้องการเลือกสุ่มจากอาร์เรย์หรือชุดข้อมูล, `Get-Random` สามารถเลือกรายการโดยตรง:

```PowerShell
# เลือกสุ่มจากอาร์เรย์
$array = 1..10
$randomItem = Get-Random -InputObject $array
Write-Output $randomItem
```

## ศึกษาลึก
คำสั่ง `Get-Random` cmdlet ใน PowerShell ใช้คลาส .NET `System.Random` เป็นพื้นฐานในการสร้างตัวเลขสุ่มข้างเคียง นี่เป็น "ข้างเคียง" เพราะว่าใช้อัลกอริทึมในการผลิตลำดับของตัวเลขที่ดูเหมือนจะเป็นสุ่มเท่านั้น สำหรับการใช้งานทั่วไป ระดับความสุ่มนี้มีเพียงพอ อย่างไรก็ตาม, สำหรับใช้งานที่ต้องการความปลอดภัยทางการเข้ารหัส  `System.Random` ไม่เหมาะสมเนื่องจากธรรมชาติที่สามารถทายได้

PowerShell และ .NET มี `System.Security.Cryptography.RNGCryptoServiceProvider` สำหรับความสุ่มที่ปลอดภัยทางการเข้ารหัสซึ่งเหมาะสมกว่าสำหรับการสร้างกุญแจการเข้ารหัสหรือการดำเนินการที่เกี่ยวข้องกับความปลอดภัยอื่นๆ:

```PowerShell
# ตัวเลขสุ่มที่ปลอดภัยทางการเข้ารหัส
$rng = [System.Security.Cryptography.RNGCryptoServiceProvider]::new()
$bytes = New-Object byte[] 4
$rng.GetBytes($bytes)
$randomNumber = [BitConverter]::ToInt32($bytes, 0)
Write-Output $randomNumber
```

ในขณะที่ `Get-Random` และ `System.Random` ตอบสนองความต้องการของการสุ่มในการเขียนสคริปต์และตรรกะการใช้งานได้อย่างกว้างขวาง, มันสำคัญที่จะเลือกเครื่องมือที่เหมาะสมสำหรับงาน, โดยเฉพาะในการใช้งานที่เน้นความปลอดภัย ที่ความสามารถในการทายล่วงหน้าอาจเป็นจุดอ่อน
