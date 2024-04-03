---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:52.648235-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Haskell \u0E08\u0E31\u0E14\
  \u0E01\u0E32\u0E23\u0E15\u0E31\u0E27\u0E40\u0E25\u0E02\u0E40\u0E0A\u0E34\u0E07\u0E0B\
  \u0E49\u0E2D\u0E19\u0E14\u0E49\u0E27\u0E22\u0E42\u0E21\u0E14\u0E39\u0E25 `Data.Complex`\
  \ \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E41\u0E19\u0E30\u0E19\
  \u0E33\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\u0E27."
lastmod: '2024-03-17T21:57:56.262079-06:00'
model: gpt-4-0125-preview
summary: "Haskell \u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E15\u0E31\u0E27\u0E40\u0E25\
  \u0E02\u0E40\u0E0A\u0E34\u0E07\u0E0B\u0E49\u0E2D\u0E19\u0E14\u0E49\u0E27\u0E22\u0E42\
  \u0E21\u0E14\u0E39\u0E25 `Data.Complex` \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E01\
  \u0E32\u0E23\u0E41\u0E19\u0E30\u0E19\u0E33\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E23\u0E27\
  \u0E14\u0E40\u0E23\u0E47\u0E27."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E15\u0E31\
  \u0E27\u0E40\u0E25\u0E02\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\u0E19"
weight: 14
---

## วิธีการ:
Haskell จัดการตัวเลขเชิงซ้อนด้วยโมดูล `Data.Complex` นี่คือการแนะนำอย่างรวดเร็ว:

```haskell
import Data.Complex

-- กำหนดตัวเลขเชิงซ้อนสองตัว
let z1 = 3 :+ 4  -- คือ 3 + 4i
let z2 = 5 :+ (-2)  -- 5 - 2i

-- การดำเนินการทางคณิตศาสตร์
let sum = z1 + z2  -- 8 :+ 2
let difference = z1 - z2  -- -2 :+ 6
let product = z1 * z2  -- 23 :+ 14
let quotient = z1 / z2  -- 0.20689655172413793 :+ 0.9655172413793104

-- คอนจูเกตเชิงซ้อน
let conjugateZ1 = conjugate z1  -- 3 :+ (-4)

-- ขนาดและเฟส
let magnitudeZ1 = magnitude z1  -- 5.0
let phaseZ1 = phase z1  -- 0.9272952180016122

-- การแปลงจากโพลาร์เป็นสี่เหลี่ยมผืนผ้า และในทางกลับกัน
let z1Polar = polar z1  -- (5.0,0.9272952180016122)
let fromPolar = mkPolar 5.0 0.9272952180016122  -- เหมือนกับ z1
```

ตัวอย่างผลลัพธ์หลังจากโหลดโค้ดข้างต้นใน GHCi อาจเป็น:

```haskell
*Main> sum
8.0 :+ 2.0
*Main> product
23.0 :+ 14.0
*Main> magnitudeZ1
5.0
```

## ศึกษาเพิ่มเติม
ตัวเลขเชิงซ้อนมีมาตั้งแต่ศตวรรษที่ 16 แต่ได้รับการยอมรับอย่างกว้างขวางในช่วงเวลาที่หลังมากขึ้น Haskell นั้นเหมือนกับภาษาอื่นๆ เพราะมีการสนับสนุนการคำนวณเชิงซ้อนเป็นอย่างดี ทำให้ง่ายต่อการทำงานกับตัวเลขเหล่านี้โดยไม่จำเป็นต้องสร้างฐานคณิตศาสตร์ใหม่

ทางเลือกอื่น ๆ รวมถึงการสร้างประเภทของตัวเลขเชิงซ้อนของคุณเอง หรือใช้ไลบรารีสำหรับสาขาเฉพาะ เช่น ควอเทอร์เนียนสำหรับกราฟิก 3 มิติ แต่สำหรับกรณีการใช้งานส่วนใหญ่ โมดูล `Data.Complex` ของ Haskell เพียงพอแล้ว

ภายใต้ฝาครอบ  `Data.Complex` เป็นเพียงแค่ประเภทของข้อมูลที่รวมค่า `Float` หรือ `Double` สองค่า เป็นตัวแทนของส่วนจริงและส่วนจินตภาพตามลำดับ มันเป็นวิธีที่ง่ายและมีประสิทธิภาพในการทำงานกับตัวเลขเชิงซ้อนบนแพลตฟอร์ม Haskell

## ดูเพิ่มเติม
ตรวจสอบทรัพยากรเหล่านี้เพื่อศึกษาเพิ่มเติมเกี่ยวกับตัวเลขเชิงซ้อนใน Haskell:

- เอกสารคู่มือ Haskell `Data.Complex` อย่างเป็นทางการ: [Hackage Data.Complex](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Complex.html)
- การดำน้ำลึกเข้าไปในประเภทตัวเลขของ Haskell: [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/starting-out#numbers)
- สำหรับการประยุกต์ใช้ สำรวจอัลกอริทึม Fast Fourier Transform ใน Haskell: [ไลบรารี Haskell FFT](https://hackage.haskell.org/package/fft)
