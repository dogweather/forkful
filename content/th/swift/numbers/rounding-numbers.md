---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:23.475119-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Swift \u0E21\u0E35\u0E27\
  \u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E1B\u0E31\u0E14\u0E40\u0E28\u0E29\u0E15\u0E31\
  \u0E27\u0E40\u0E25\u0E02\u0E2B\u0E25\u0E32\u0E22\u0E27\u0E34\u0E18\u0E35 \u0E19\u0E35\
  \u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07."
lastmod: '2024-03-17T21:57:56.556837-06:00'
model: gpt-4-0125-preview
summary: "Swift \u0E21\u0E35\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E1B\u0E31\
  \u0E14\u0E40\u0E28\u0E29\u0E15\u0E31\u0E27\u0E40\u0E25\u0E02\u0E2B\u0E25\u0E32\u0E22\
  \u0E27\u0E34\u0E18\u0E35 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\
  \u0E22\u0E48\u0E32\u0E07."
title: "\u0E01\u0E32\u0E23\u0E1B\u0E31\u0E14\u0E40\u0E28\u0E29\u0E02\u0E2D\u0E07\u0E15\
  \u0E31\u0E27\u0E40\u0E25\u0E02"
weight: 13
---

## วิธีการ:
Swift มีวิธีการปัดเศษตัวเลขหลายวิธี นี่คือตัวอย่าง:

```Swift
let original = 3.14159

// การปัดเศษแบบมาตรฐาน
let standardRounded = round(original) // 3.0

// การปัดเศษเข้าหาทศนิยมที่เฉพาะเจาะจง
let decimalRounded = Double(round(original * 1000) / 1000) // 3.142

// การปัดเศษลง
let roundedDown = floor(original) // 3.0

// การปัดเศษขึ้น
let roundedUp = ceil(original) // 4.0

print("มาตรฐาน: \(standardRounded), ทศนิยม: \(decimalRounded), ปัดลง: \(roundedDown), ปัดขึ้น: \(roundedUp)")
```

ผลลัพธ์: `มาตรฐาน: 3.0, ทศนิยม: 3.142, ปัดลง: 3.0, ปัดขึ้น: 4.0`

## ค้นลึก
ในประวัติศาสตร์, การปัดเศษเป็นแนวคิดทางคณิตศาสตร์ที่มีมาก่อนคอมพิวเตอร์, เป็นสิ่งสำคัญในการค้าและวิทยาศาสตร์ กรอบงาน `Foundation` ของ Swift มอบฟังก์ชันการปัดเศษอย่างครอบคลุม:

- `round(_: )` คือการปัดเศษแบบครึ่งหนึ่งขึ้น
- `floor(_: )` และ `ceil(_: )` จัดการกับการปัดเศษตามทิศทาง
- `rounded(.up/.down/.toNearestOrAwayFromZero)` ให้การควบคุมที่ละเอียดยิ่งขึ้นด้วย enum กฎการปัดเศษ

ควรระวังเกี่ยวกับประเภท `Decimal` สำหรับการคำนวณทางการเงินที่แม่นยำ, ซึ่งหลีกเลี่ยงข้อผิดพลาดจากจุดลอยทศนิยม นอกจากนี้, สำรวจ `NSDecimalNumber` สำหรับความเข้ากันได้กับ Objective-C

## ดูเพิ่มเติม
- มาตรฐาน IEEE สำหรับการคำนวณจุดลอยตัว (IEEE 754): [IEEE 754](https://ieeexplore.ieee.org/document/4610935)
