---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:32.327998-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: PowerShell \u0E21\u0E35\u0E04\u0E27\
  \u0E32\u0E21\u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E19\u0E01\u0E32\u0E23\u0E08\
  \u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E44\
  \u0E14\u0E49\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E14\u0E35 \u0E40\u0E23\u0E32\u0E43\u0E0A\
  \u0E49\u0E40\u0E21\u0E18\u0E2D\u0E14 `.ToLower()` \u0E14\u0E31\u0E07\u0E19\u0E35\
  \u0E49."
lastmod: '2024-03-17T21:57:56.426422-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u0E21\u0E35\u0E04\u0E27\u0E32\u0E21\u0E2A\u0E32\u0E21\u0E32\u0E23\
  \u0E16\u0E43\u0E19\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\
  \u0E1A\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E44\u0E14\u0E49\u0E2D\u0E22\u0E48\u0E32\u0E07\
  \u0E14\u0E35 \u0E40\u0E23\u0E32\u0E43\u0E0A\u0E49\u0E40\u0E21\u0E18\u0E2D\u0E14\
  \ `.ToLower()` \u0E14\u0E31\u0E07\u0E19\u0E35\u0E49."
title: "\u0E41\u0E1B\u0E25\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\
  \u0E15\u0E31\u0E27\u0E40\u0E25\u0E47\u0E01"
weight: 4
---

## วิธีทำ:
PowerShell มีความสามารถในการจัดการกับสตริงได้อย่างดี เราใช้เมธอด `.ToLower()` ดังนี้:

```PowerShell
$string = "HELLO, World!"
$lowerCaseString = $string.ToLower()
$lowerCaseString
```

ผลลัพธ์:

```
hello, world!
```

หรือลองใช้เมธอด `ToLowerInvariant()` เมื่อความเป็นสากลไม่ควรมีผลต่อการแปลง:

```PowerShell
$string = "HELLO, World!"
$lowerCaseInvariant = $string.ToLowerInvariant()
$lowerCaseInvariant
```

ผลลัพธ์:

```
hello, world!
```

## ลงลึก
มีครั้งหนึ่งที่การไม่แยกตัวพิมพ์ใหญ่เล็กนั้นทั่วไปมากในภาษาโปรแกรม ใน PowerShell ที่มีรากฐานมาจาก .NET, สตริงเป็นออบเจกต์ที่มีเมธอดในตัวสำหรับการจัดการ เมื่อเราใช้ `.ToLower()` เรากำลังเรียกใช้เมธอดที่จัดการการแปลงให้เรา

มีวิธีอื่นในการทำสิ่งนี้หรือไม่? แน่นอน คุณสามารถใช้:

- ลูป `for` โดยเข้าไปที่แต่ละอักขระ และเปลี่ยนตัวพิมพ์ด้วยตนเอง
- Regular Expressions ผ่านฟังก์ชัน `-replace`
- การแปลงที่เฉพาะเจาะจงต่อวัฒนธรรมโดยใช้รูปแบบโอเวอร์โหลดของ `.ToLower()`
  
ทำไมต้องใช้วัฒนธรรมที่ไม่แปรผันด้วย `ToLowerInvariant()`? สำคัญมากสำหรับผลลัพธ์ที่มีความสอดคล้องกันในภูมิภาคที่ต่างกันที่การตีความของสิ่งที่เป็น "ตัวพิมพ์เล็ก" อาจแตกต่างกัน

## ดูเพิ่มเติม
สำหรับการผจญภัยอย่างละเอียดในการจัดการสตริง ดูที่ลิงก์เหล่านี้:

- [คลาสสตริง .NET](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=net-6.0)
