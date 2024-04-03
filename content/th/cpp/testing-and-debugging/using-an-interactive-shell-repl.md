---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:40.190661-06:00
description: "REPL (Read-Eval-Print-Loop) \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E20\u0E32\
  \u0E1E\u0E41\u0E27\u0E14\u0E25\u0E49\u0E2D\u0E21\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\
  \u0E22\u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E41\u0E1A\u0E1A\u0E07\u0E48\
  \u0E32\u0E22 \u0E46 \u0E41\u0E15\u0E48\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E44\
  \u0E14\u0E49 \u0E19\u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E43\u0E0A\u0E49\u0E21\
  \u0E31\u0E19\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\u0E14\u0E25\u0E2D\u0E07\u0E20\u0E32\
  \u0E29\u0E32\u0E41\u0E1A\u0E1A\u0E40\u0E23\u0E35\u0E22\u0E25\u0E44\u0E17\u0E21\u0E4C\
  , \u0E07\u0E32\u0E19\u0E40\u0E25\u0E47\u0E01\u0E46 \u0E19\u0E49\u0E2D\u0E22\u0E46\
  ,\u2026"
lastmod: '2024-03-17T21:57:56.523152-06:00'
model: gpt-4-0125-preview
summary: "REPL (Read-Eval-Print-Loop) \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E20\u0E32\u0E1E\
  \u0E41\u0E27\u0E14\u0E25\u0E49\u0E2D\u0E21\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\
  \u0E19\u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E41\u0E1A\u0E1A\u0E07\u0E48\u0E32\
  \u0E22 \u0E46 \u0E41\u0E15\u0E48\u0E42\u0E15\u0E49\u0E15\u0E2D\u0E1A\u0E44\u0E14\
  \u0E49 \u0E19\u0E31\u0E01\u0E1E\u0E31\u0E12\u0E19\u0E32\u0E43\u0E0A\u0E49\u0E21\u0E31\
  \u0E19\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\u0E14\u0E25\u0E2D\u0E07\u0E20\u0E32\u0E29\
  \u0E32\u0E41\u0E1A\u0E1A\u0E40\u0E23\u0E35\u0E22\u0E25\u0E44\u0E17\u0E21\u0E4C,\
  \ \u0E07\u0E32\u0E19\u0E40\u0E25\u0E47\u0E01\u0E46 \u0E19\u0E49\u0E2D\u0E22\u0E46\
  , \u0E2B\u0E23\u0E37\u0E2D\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E40\u0E02\u0E49\u0E32\
  \u0E43\u0E08\u0E41\u0E19\u0E27\u0E04\u0E34\u0E14\u0E43\u0E2B\u0E21\u0E48\u0E46 \u0E42\
  \u0E14\u0E22\u0E44\u0E21\u0E48\u0E15\u0E49\u0E2D\u0E07\u0E2A\u0E23\u0E49\u0E32\u0E07\
  \u0E41\u0E2D\u0E1B\u0E1E\u0E25\u0E34\u0E40\u0E04\u0E0A\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E40\u0E15\u0E47\u0E21\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49 Shell \u0E41\u0E1A\u0E1A\u0E42\u0E15\u0E49\
  \u0E15\u0E2D\u0E1A (REPL)"
weight: 34
---

## วิธีการ:
C++ ไม่มี REPL ที่มีตั้งแต่เริ่มต้น, แต่เครื่องมือเช่น Cling สามารถทำงานดังกล่าวได้ นี่คือวิธีใช้ Cling เพื่อคำนวณผลรวมของสองตัวเลข:

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 7;
    std::cout << "The sum is: " << a + b << std::endl;
    return 0;
}

// ผลลัพธ์:
// The sum is: 12
```

เริ่มต้น Cling และป้อนโค้ดทีละบรรทัด, สังเกตุผลลัพธ์หลังจากทำแต่ละคำสั่ง เป็นการตอบสนองทันที, ไม่ต้องคอมไพล์

## ทำความลึกซึ้ง
REPLs เป็นที่นิยมสำหรับภาษาเช่น Python หรือ Lisp, และมีมาตั้งแต่ปี 1960 สำหรับ C++, ภาษาที่ต้องคอมไพล์, ความคิดนี้ไม่เข้ากันได้ตามธรรมชาติ, ซึ่งเป็นเหตุผลว่าทำไมเครื่องมือเช่น Cling ถึงมีอยู่—พวกเขาตีความ C++ แบบทันที ทางเลือกอื่น ๆ รวมถึงคอมไพเลอร์ออนไลน์หรือโปรแกรมทดสอบขนาดเล็กที่คอมไพล์แบบดั้งเดิม Cling สร้างบนพื้นฐานของ LLVM และ Clang, ให้สะพานให้ C++ สามารถใช้ในรูปแบบที่ตีความได้

## ดูเพิ่มเติม
- [Cling](https://root.cern/cling/): ตีความ C++ แบบโต้ตอบ, สร้างบนพื้นฐานของไลบรารี LLVM และ Clang
- [Jupyter Notebooks](https://jupyter.org/): นำเสนอ shell โต้ตอบภายในสภาพแวดล้อม notebook, สนับสนุน C++ ผ่านเคอร์เนล xeus-cling
- [LLVM](https://llvm.org/): ชุดของเทคโนโลยีคอมไพเลอร์และชุดเครื่องมือที่สามารถใช้ซ้ำได้และเป็นโมดูลาร์, ซึ่ง Cling สร้างขึ้นมา
