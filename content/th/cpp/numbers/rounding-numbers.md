---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:21.705719-06:00
description: "\u0E01\u0E32\u0E23\u0E1B\u0E31\u0E14\u0E40\u0E28\u0E29\u0E15\u0E31\u0E27\
  \u0E40\u0E25\u0E02\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E1B\
  \u0E23\u0E31\u0E1A\u0E04\u0E48\u0E32\u0E43\u0E2B\u0E49\u0E43\u0E01\u0E25\u0E49\u0E40\
  \u0E04\u0E35\u0E22\u0E07\u0E01\u0E31\u0E1A\u0E08\u0E33\u0E19\u0E27\u0E19\u0E40\u0E15\
  \u0E47\u0E21\u0E2B\u0E23\u0E37\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E41\u0E21\u0E48\u0E19\
  \u0E22\u0E33\u0E17\u0E35\u0E48\u0E01\u0E33\u0E2B\u0E19\u0E14 \u0E19\u0E31\u0E01\u0E1E\
  \u0E31\u0E12\u0E19\u0E32\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E40\u0E23\u0E37\u0E48\u0E2D\
  \u0E07\u0E07\u0E48\u0E32\u0E22\u0E02\u0E36\u0E49\u0E19,\u2026"
lastmod: '2024-03-17T21:57:56.516735-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E1B\u0E31\u0E14\u0E40\u0E28\u0E29\u0E15\u0E31\u0E27\
  \u0E40\u0E25\u0E02\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E1B\
  \u0E23\u0E31\u0E1A\u0E04\u0E48\u0E32\u0E43\u0E2B\u0E49\u0E43\u0E01\u0E25\u0E49\u0E40\
  \u0E04\u0E35\u0E22\u0E07\u0E01\u0E31\u0E1A\u0E08\u0E33\u0E19\u0E27\u0E19\u0E40\u0E15\
  \u0E47\u0E21\u0E2B\u0E23\u0E37\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E41\u0E21\u0E48\u0E19\
  \u0E22\u0E33\u0E17\u0E35\u0E48\u0E01\u0E33\u0E2B\u0E19\u0E14 \u0E19\u0E31\u0E01\u0E1E\
  \u0E31\u0E12\u0E19\u0E32\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E40\u0E23\u0E37\u0E48\u0E2D\
  \u0E07\u0E07\u0E48\u0E32\u0E22\u0E02\u0E36\u0E49\u0E19,\u2026"
title: "\u0E01\u0E32\u0E23\u0E1B\u0E31\u0E14\u0E40\u0E28\u0E29\u0E02\u0E2D\u0E07\u0E15\
  \u0E31\u0E27\u0E40\u0E25\u0E02"
---

{{< edit_this_page >}}

## อะไร & ทำไม?
การปัดเศษตัวเลขหมายถึงการปรับค่าให้ใกล้เคียงกับจำนวนเต็มหรือความแม่นยำที่กำหนด นักพัฒนาทำเช่นนี้เพื่อทำให้เรื่องง่ายขึ้น, ให้สอดคล้องกับข้อจำกัดในโลกความจริง, หรือเพื่อปรับปรุงประสิทธิภาพโดยการละทิ้งความแม่นยำที่เกินความจำเป็น

## วิธีการ:
C++ เสนอวิธีการปัดเศษตัวเลขหลายวิธี เช่น `floor()`, `ceil()`, และ `round()`:

```C++
#include <iostream>
#include <cmath> // สำหรับฟังก์ชันการปัดเศษ

int main() {
    double num = 3.14;

    std::cout << "floor: " << std::floor(num) << "\n"; // ผลลัพธ์: floor: 3
    std::cout << "ceil: " << std::ceil(num) << "\n";   // ผลลัพธ์: ceil: 4
    std::cout << "round: " << std::round(num) << "\n"; // ผลลัพธ์: round: 3

    // สำหรับความแม่นยำที่ตายตัว เช่น การปัดเศษไปเป็นสองทศนิยม:
    double precise_num = 3.146;
    double multiplier = 100.0;
    double rounded = std::round(precise_num * multiplier) / multiplier;

    std::cout << "rounded to two decimals: " << rounded << "\n"; // ผลลัพธ์: rounded to two decimals: 3.15

    return 0;
}
```

## ลงลึก
ก่อน C++11, การปัดเศษขึ้นอาศัยเทคนิคแบบดั้งเดิมหรือไลบรารี่ที่ไม่เป็นมาตรฐาน ในปัจจุบัน, `<cmath>` ให้วิธีการที่มั่นคง `floor()` ปัดลง, `ceil()` ปัดขึ้น, ขณะที่ `round()` ไปที่จำนวนเต็มที่ใกล้ที่สุด, แม้แต่การจัดการกับกรณีการตัดสินใจ (กรณี 0.5) โดยปัดไปที่จำนวนคู่

การเข้าใจพฤติกรรมของฟังก์ชันเหล่านี้เป็นสิ่งสำคัญ เช่น ตัวเลขลบอาจทำให้คุณตกใจ (`std::round(-2.5)` ให้ผลลัพธ์ `-2.0`)

มีทางเลือกอื่นไหม? การแปลงเป็น int หลังจากที่เพิ่ม 0.5 สำหรับตัวเลขบวกเป็นเทคนิคแบบคลาสสิก แต่ผิดพลาดกับตัวเลขลบและไม่มีความเป็นกลางทางประเภท ไลบรารีเช่น Boost สามารถเสนอวิธีการที่มีความละเอียดอ่อนมากขึ้น ขณะที่ส่วนขยายภาษาหรือ intrinsic ของตัวแปลสามารถปรับให้เหมาะสมกับฮาร์ดแวร์เฉพาะได้

## ดูเพิ่มเติม
- อ้างอิง C++ สำหรับ `<cmath>`: https://en.cppreference.com/w/cpp/header/cmath
- มาตรฐาน IEEE สำหรับการคำนวณจุดลอยตัว (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- ไลบรารีการแปลงตัวเลขของ Boost: https://www.boost.org/doc/libs/release/libs/numeric/conversion/
