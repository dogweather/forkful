---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:59.356027-06:00
description: "\u0E01\u0E32\u0E23\u0E1B\u0E31\u0E14\u0E40\u0E28\u0E29\u0E15\u0E31\u0E27\
  \u0E40\u0E25\u0E02\u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E15\
  \u0E31\u0E27\u0E40\u0E25\u0E02\u0E43\u0E2B\u0E49\u0E43\u0E01\u0E25\u0E49\u0E40\u0E04\
  \u0E35\u0E22\u0E07\u0E01\u0E31\u0E1A\u0E08\u0E33\u0E19\u0E27\u0E19\u0E40\u0E15\u0E47\
  \u0E21\u0E2B\u0E23\u0E37\u0E2D\u0E17\u0E28\u0E19\u0E34\u0E22\u0E21\u0E17\u0E35\u0E48\
  \u0E23\u0E30\u0E1A\u0E38\u0E21\u0E32\u0E01\u0E17\u0E35\u0E48\u0E2A\u0E38\u0E14 \u0E42\
  \u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E1B\u0E31\u0E14\
  \u0E40\u0E28\u0E29\u0E15\u0E31\u0E27\u0E40\u0E25\u0E02\u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E04\u0E27\u0E1A\u0E04\u0E38\u0E21\u0E04\u0E27\u0E32\u0E21\u0E41\u0E21\u0E48\u0E19\
  \u0E22\u0E33,\u2026"
lastmod: '2024-03-17T21:57:56.263010-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E1B\u0E31\u0E14\u0E40\u0E28\u0E29\u0E15\u0E31\u0E27\
  \u0E40\u0E25\u0E02\u0E04\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E15\
  \u0E31\u0E27\u0E40\u0E25\u0E02\u0E43\u0E2B\u0E49\u0E43\u0E01\u0E25\u0E49\u0E40\u0E04\
  \u0E35\u0E22\u0E07\u0E01\u0E31\u0E1A\u0E08\u0E33\u0E19\u0E27\u0E19\u0E40\u0E15\u0E47\
  \u0E21\u0E2B\u0E23\u0E37\u0E2D\u0E17\u0E28\u0E19\u0E34\u0E22\u0E21\u0E17\u0E35\u0E48\
  \u0E23\u0E30\u0E1A\u0E38\u0E21\u0E32\u0E01\u0E17\u0E35\u0E48\u0E2A\u0E38\u0E14 \u0E42\
  \u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E1B\u0E31\u0E14\
  \u0E40\u0E28\u0E29\u0E15\u0E31\u0E27\u0E40\u0E25\u0E02\u0E40\u0E1E\u0E37\u0E48\u0E2D\
  \u0E04\u0E27\u0E1A\u0E04\u0E38\u0E21\u0E04\u0E27\u0E32\u0E21\u0E41\u0E21\u0E48\u0E19\
  \u0E22\u0E33,\u2026"
title: "\u0E01\u0E32\u0E23\u0E1B\u0E31\u0E14\u0E40\u0E28\u0E29\u0E02\u0E2D\u0E07\u0E15\
  \u0E31\u0E27\u0E40\u0E25\u0E02"
weight: 13
---

## อะไร & ทำไม?

การปัดเศษตัวเลขคือการปรับตัวเลขให้ใกล้เคียงกับจำนวนเต็มหรือทศนิยมที่ระบุมากที่สุด โปรแกรมเมอร์ปัดเศษตัวเลขเพื่อควบคุมความแม่นยำ, ปรับผลลัพธ์ให้เหมาะสมสำหรับการนำเสนอต่อผู้ใช้ หรือลดต้นทุนการคำนวณสำหรับการดำเนินการจุดลอยตัว

## วิธีการ:

Haskell ใช้ฟังก์ชั่น `round`, `ceiling`, `floor`, และ `truncate` จาก `Prelude` สำหรับการปัดเศษ

```haskell
import Prelude

main :: IO ()
main = do
  let num = 3.567
  print $ round num    -- 4
  print $ ceiling num  -- 4
  print $ floor num    -- 3
  print $ truncate num -- 3
  
  -- การปัดเศษไปยังทศนิยมที่เฉพาะเจาะจงไม่ใช่ส่วนหนึ่งของ Prelude
  -- นี่คือฟังก์ชันที่กำหนดเอง:
  let roundTo n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)
  print $ roundTo 1 num -- 3.6
```

## การศึกษาลึก

ในอดีต, การปัดเศษมีความสำคัญในการวิเคราะห์เชิงตัวเลขและวิทยาการคอมพิวเตอร์ เพราะเป็นสิ่งที่สำคัญในการลดการสะสมข้อผิดพลาดในการคำนวณ โดยเฉพาะก่อนที่การแทนค่าจุดลอยตัวจะมีการมาตรฐานสากลตาม IEEE 754

ควรปัดเศษไปยังอะไร? `round` จะนำคุณไปยังจำนวนเต็มที่ใกล้ที่สุด—ขึ้นหรือลง `ceiling` และ `floor` จะปัดเศษขึ้นหรือลงไปยังจำนวนเต็มที่ใกล้ที่สุดตามลำดับ ขณะที่ `truncate` เพียงแค่ตัดทศนิยมทิ้ง

ตัวเลือกอื่นๆ นอกเหนือจากฟังก์ชันเหล่านี้อาจเกี่ยวข้องกับตรรกะที่กำหนดเอง เช่น `roundTo` ของเรา หรือคุณอาจใช้ไลบรารี (เช่น Data.Fixed) สำหรับความต้องการที่ซับซ้อนมากขึ้น

ระวังผลลัพธ์ที่ไม่คาดคิดเนื่องจากวิธีที่ Haskell จัดการกับกรณีครึ่งทางใน `round` (มันปัดเศษไปยังจำนวนคู่ที่ใกล้ที่สุด)

## ดูเพิ่มเติม

- เอกสารของ Haskell Prelude สำหรับฟังก์ชั่นการปัดเศษ: https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html
- Haskell Wiki เกี่ยวกับคณิตศาสตร์จุดลอยตัว: https://wiki.haskell.org/Floating_point_arithmetic
- มาตรฐาน IEEE 754-2008 สำหรับข้อมูลเพิ่มเติมว่าการแทนค่าจุดลอยตัวจัดการอย่างไรในหลายภาษา: https://ieeexplore.ieee.org/document/4610935
