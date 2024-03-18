---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:13.218137-06:00
description: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E23\u0E30\u0E40\u0E1A\u0E35\u0E22\
  \u0E1A\u0E42\u0E04\u0E49\u0E14\u0E40\u0E02\u0E49\u0E32\u0E01\u0E31\u0E1A\u0E1F\u0E31\
  \u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E43\u0E19\u0E20\u0E32\u0E29\u0E32 Haskell\
  \ \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E41\u0E1A\u0E48\
  \u0E07\u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E2D\u0E2D\u0E01\
  \u0E40\u0E1B\u0E47\u0E19\u0E1A\u0E25\u0E47\u0E2D\u0E01\u0E17\u0E35\u0E48\u0E2A\u0E32\
  \u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\u0E0B\u0E49\u0E33\u0E44\u0E14\u0E49 \u0E0B\
  \u0E36\u0E48\u0E07\u0E21\u0E35\u0E0A\u0E37\u0E48\u0E2D\u0E40\u0E23\u0E35\u0E22\u0E01\
  \ \u0E17\u0E33\u0E44\u0E21\u0E16\u0E36\u0E07\u0E17\u0E33\u0E2D\u0E22\u0E48\u0E32\
  \u0E07\u0E19\u0E31\u0E49\u0E19?\u2026"
lastmod: '2024-03-17T21:57:56.273658-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E23\u0E30\u0E40\u0E1A\u0E35\u0E22\
  \u0E1A\u0E42\u0E04\u0E49\u0E14\u0E40\u0E02\u0E49\u0E32\u0E01\u0E31\u0E1A\u0E1F\u0E31\
  \u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E43\u0E19\u0E20\u0E32\u0E29\u0E32 Haskell\
  \ \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E41\u0E1A\u0E48\
  \u0E07\u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E2D\u0E2D\u0E01\
  \u0E40\u0E1B\u0E47\u0E19\u0E1A\u0E25\u0E47\u0E2D\u0E01\u0E17\u0E35\u0E48\u0E2A\u0E32\
  \u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\u0E0B\u0E49\u0E33\u0E44\u0E14\u0E49 \u0E0B\
  \u0E36\u0E48\u0E07\u0E21\u0E35\u0E0A\u0E37\u0E48\u0E2D\u0E40\u0E23\u0E35\u0E22\u0E01\
  \ \u0E17\u0E33\u0E44\u0E21\u0E16\u0E36\u0E07\u0E17\u0E33\u0E2D\u0E22\u0E48\u0E32\
  \u0E07\u0E19\u0E31\u0E49\u0E19?\u2026"
title: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E23\u0E30\u0E40\u0E1A\u0E35\u0E22\u0E1A\
  \u0E42\u0E04\u0E49\u0E14\u0E40\u0E02\u0E49\u0E32\u0E44\u0E1B\u0E43\u0E19\u0E1F\u0E31\
  \u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19"
---

{{< edit_this_page >}}

## อะไร & ทำไม?
การจัดระเบียบโค้ดเข้ากับฟังก์ชันในภาษา Haskell หมายถึงการแบ่งโค้ดของคุณออกเป็นบล็อกที่สามารถใช้ซ้ำได้ ซึ่งมีชื่อเรียก ทำไมถึงทำอย่างนั้น? มันช่วยให้โค้ดของคุณไม่ซ้ำซาก (DRY - Don't Repeat Yourself), ทำให้โค้ดอ่านง่ายขึ้น และง่ายต่อการตรวจสอบแก้ไข

## วิธีการ:
นี่คือวิธีที่คุณสามารถเขียนและใช้งานฟังก์ชันใน Haskell:

```Haskell
-- การประกาศฟังก์ชันง่ายๆ เพื่อบวกตัวเลขสองตัว
addNumbers :: Int -> Int -> Int
addNumbers x y = x + y

-- การใช้งานฟังก์ชัน
main = print (addNumbers 3 5)
```

ผลลัพธ์:
```
8
```

คุณยังสามารถสร้างฟังก์ชันระดับสูงได้:

```Haskell
-- รับฟังก์ชันหนึ่งฟังก์ชันและนำไปใช้สองครั้งกับบางอย่าง
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- การใช้งาน applyTwice กับฟังก์ชันไม่ระบุชื่อ
main = print (applyTwice (*2) 5)
```

ผลลัพธ์:
```
20
```

## แง่ลึก
Haskell, ภาษาที่เป็นฟังก์ชันอย่างบริสุทธิ์, ถือฟังก์ชันเป็นสมาชิกชั้นหนึ่ง ในแง่ประวัติศาสตร์, นี่มีรากฐานมาจากแคลคูลัสแลมบ์ดา, กรอบการทำงานพื้นฐานในการคำนวณ ไม่เหมือนกับภาษาคำสั่งที่ฟังก์ชันเป็นลำดับของคำสั่ง, ใน Haskell, ฟังก์ชันเป็นนิพจน์ที่อธิบายความสัมพันธ์ระหว่างข้อมูล

มีทางเลือกอื่นในการเขียนฟังก์ชันดิบสำหรับการใช้ซ้ำ พิจารณาใช้ typeclasses สำหรับ polymorphism หรือการใช้งานโมดูลเพื่อกลุ่มฟังก์ชันที่เกี่ยวข้องกัน Haskell การประเมินค่าแบบเลือกสรรยังส่งผลต่อการทำงานของฟังก์ชัน—ฟังก์ชันจะไม่ถูกประเมินค่าจนกระทั่งผลลัพธ์เป็นที่ต้องการ, ซึ่งอาจส่งผลต่อการพิจารณาประสิทธิภาพ

## ดูเพิ่มเติม
- Official Haskell Documentation: https://www.haskell.org/documentation/
- "Learn You a Haskell for Great Good!" โดย Miran Lipovača, หนังสือที่เหมาะสำหรับผู้เริ่มต้น: http://learnyouahaskell.com/
- "Real World Haskell" โดย Bryan O'Sullivan, Don Stewart, และ John Goerzen: http://book.realworldhaskell.org/
