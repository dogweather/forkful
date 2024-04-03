---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:28.883261-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Elm \u0E44\u0E21\u0E48\u0E21\
  \u0E35\u0E01\u0E32\u0E23\u0E2A\u0E19\u0E31\u0E1A\u0E2A\u0E19\u0E38\u0E19\u0E08\u0E33\
  \u0E19\u0E27\u0E19\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\u0E19\u0E41\u0E1A\u0E1A\u0E43\
  \u0E19\u0E15\u0E31\u0E27 \u0E14\u0E31\u0E07\u0E19\u0E31\u0E49\u0E19\u0E04\u0E38\u0E13\
  \u0E08\u0E30\u0E15\u0E49\u0E2D\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E1B\u0E23\u0E30\
  \u0E40\u0E20\u0E17\u0E41\u0E25\u0E30\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\
  \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E40\u0E2D\u0E07 \u0E19\u0E35\u0E48\u0E04\u0E37\
  \u0E2D\u0E02\u0E31\u0E49\u0E19\u0E15\u0E2D\u0E19\u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\
  \u0E07\u0E04\u0E48\u0E32\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E23\u0E27\u0E14\u0E40\u0E23\
  \u0E47\u0E27."
lastmod: '2024-03-17T21:57:56.122322-06:00'
model: gpt-4-0125-preview
summary: "Elm \u0E44\u0E21\u0E48\u0E21\u0E35\u0E01\u0E32\u0E23\u0E2A\u0E19\u0E31\u0E1A\
  \u0E2A\u0E19\u0E38\u0E19\u0E08\u0E33\u0E19\u0E27\u0E19\u0E0B\u0E31\u0E1A\u0E0B\u0E49\
  \u0E2D\u0E19\u0E41\u0E1A\u0E1A\u0E43\u0E19\u0E15\u0E31\u0E27 \u0E14\u0E31\u0E07\u0E19\
  \u0E31\u0E49\u0E19\u0E04\u0E38\u0E13\u0E08\u0E30\u0E15\u0E49\u0E2D\u0E07\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E1B\u0E23\u0E30\u0E40\u0E20\u0E17\u0E41\u0E25\u0E30\u0E1F\u0E31\
  \u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13\u0E40\u0E2D\
  \u0E07 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E02\u0E31\u0E49\u0E19\u0E15\u0E2D\u0E19\
  \u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32\u0E2D\u0E22\u0E48\u0E32\
  \u0E07\u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\u0E27."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E15\u0E31\
  \u0E27\u0E40\u0E25\u0E02\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\u0E19"
weight: 14
---

## วิธีการ:
Elm ไม่มีการสนับสนุนจำนวนซับซ้อนแบบในตัว ดังนั้นคุณจะต้องสร้างประเภทและฟังก์ชันของคุณเอง นี่คือขั้นตอนการตั้งค่าอย่างรวดเร็ว:

```Elm
type alias Complex =
    { real : Float, imaginary : Float }

add : Complex -> Complex -> Complex
add a b =
    { real = a.real + b.real, imaginary = a.imaginary + b.imaginary }

-- ตัวอย่างการใช้งาน:
a = { real = 3, imaginary = 2 }
b = { real = 1, imaginary = -4 }

sum = add a b
-- sum เป็น { real = 4.0, imaginary = -2.0 }
```

## การศึกษาเจาะลึก
ในอดีต จำนวนซับซ้อนไม่ได้ถูกยอมรับเสมอไป พวกมันกลายเป็นตัวเปลี่ยนเกมในศตวรรษที่ 16 เพื่อแก้สมการลูกบาศก์ ภาษาอื่น ๆ เช่น Python นำเสนอการสนับสนุนจำนวนซับซ้อนแบบในตัวด้วยการดำเนินการทันทีออกจากกล่อง Elm ต้องการวิธีการ DIY ตามที่คุณเห็น แต่คุณสามารถทำให้มันซับซ้อนมากเท่าที่ต้องการ สร้างการคูณ การหาร และการดำเนินการอื่น ๆ ปรับประสิทธิภาพได้

## ดูเพิ่มเติม
- คู่มืออย่างเป็นทางการของ Elm: https://package.elm-lang.org/ เพื่อการสร้างประเภทที่กำหนดเองและการเรียนรู้พื้นฐานของ Elm
- ผู้ที่สนใจประวัติศาสตร์คณิตศาสตร์อาจดู "An Imaginary Tale" โดย Paul J. Nahin เพื่อการเดินทางของจำนวนซับซ้อนผ่านเวลา
- ดำดิ่งสู่ความท้าทายที่เน้นการเขียนโปรแกรมทางคณิตศาสตร์บน Project Euler (https://projecteuler.net) เพื่อนำความเชี่ยวชาญด้านจำนวนซับซ้อนของคุณไปใช้
