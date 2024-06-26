---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:38.705225-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: C++ \u0E2A\u0E32\u0E21\u0E32\
  \u0E23\u0E16\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E23\u0E48\u0E27\u0E21\u0E01\u0E31\
  \u0E1A\u0E15\u0E31\u0E27\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C\
  \u0E40\u0E0A\u0E48\u0E19 GDB \u0E2B\u0E23\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E14\u0E35\
  \u0E1A\u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C\u0E02\u0E2D\u0E07 Visual Studio\
  \ \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\
  \u0E07\u0E40\u0E25\u0E47\u0E01\u0E19\u0E49\u0E2D\u0E22\u0E19\u0E48\u0E32\u0E23\u0E31\
  \u0E1A\u0E1B\u0E23\u0E30\u0E17\u0E32\u0E19\u0E14\u0E49\u0E27\u0E22 GDB."
lastmod: '2024-03-17T21:57:56.525928-06:00'
model: gpt-4-0125-preview
summary: "C++ \u0E2A\u0E32\u0E21\u0E32\u0E23\u0E16\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\
  \u0E23\u0E48\u0E27\u0E21\u0E01\u0E31\u0E1A\u0E15\u0E31\u0E27\u0E14\u0E35\u0E1A\u0E31\
  \u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C\u0E40\u0E0A\u0E48\u0E19 GDB \u0E2B\u0E23\u0E37\
  \u0E2D\u0E15\u0E31\u0E27\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C\
  \u0E02\u0E2D\u0E07 Visual Studio \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\
  \u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E40\u0E25\u0E47\u0E01\u0E19\u0E49\u0E2D\u0E22\
  \u0E19\u0E48\u0E32\u0E23\u0E31\u0E1A\u0E1B\u0E23\u0E30\u0E17\u0E32\u0E19\u0E14\u0E49\
  \u0E27\u0E22 GDB."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E07\u0E32\u0E19\u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\u0E01\u0E2D\u0E23\u0E4C"
weight: 35
---

## วิธีการ:
C++ สามารถใช้งานร่วมกับตัวดีบักเกอร์เช่น GDB หรือตัวดีบักเกอร์ของ Visual Studio นี่คือตัวอย่างเล็กน้อยน่ารับประทานด้วย GDB:

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 0;
    int c = a / b; // อุ๊ปส์, การหารด้วยศูนย์!
    std::cout << c << std::endl;
    return 0;
}

// คอมไพล์ด้วย:
// g++ -g -o my_program my_program.cpp

// รันด้วยตัวดีบักเกอร์:
// gdb ./my_program
```

เมื่อคุณเริ่มต้นใช้งาน GDB คุณสามารถตั้งจุดหยุดการทำงาน (breakpoints), เดินผ่านโค้ดของคุณ, ตรวจสอบตัวแปร, และอื่นๆ อีกมากมาย ถ้าคุณรันสิ่งที่กล่าวไว้ด้านบน คุณควรจะเห็นโปรแกรมของคุณเกิดการล่มเนื่องจากการหารด้วยศูนย์

## ลงลึก
การดีบักมีรากฐานมาจากวันแรกๆ ของการเขียนโปรแกรม ที่ต้องการการลบบัก (แมลง!) ออกจากฮาร์ดแวร์จริงๆ ตั้งแต่นั้นมา เครื่องมือการดีบักได้พัฒนาเป็นซอฟต์แวร์ที่ซับซ้อนและทรงพลัง มีความสำคัญต่อการพัฒนา

ทางเลือกสำหรับ GDB สำหรับ C++ รวมถึง LLDB และตัวดีบักเกอร์ที่ผสานรวมใน IDE เช่น Visual Studio, CLion หรือ Eclipse เหล่าสภาพแวดล้อมสมัยใหม่นี้ให้หน้าตาที่เป็นกราฟิก ทำให้การดีบักดูน่าจะเข้าถึงได้ง่ายขึ้น

รายละเอียดการประยุกต์ใช้งานตัวดีบักเกอร์มักจะขึ้นอยู่กับสภาพแวดล้อมการพัฒนาของคุณ:

- ตัวดีบักเกอร์แบบคอมมานด์ไลน์ (GDB, LLDB) ต้องการความคุ้นเคยกับคำสั่งเทอร์มินัลและมักจะมีความชันของการเรียนรู้ที่สูงกว่า
- ตัวดีบักเกอร์แบบกราฟิกทำให้กระบวนการง่ายขึ้นโดยอนุญาตให้มีการโต้ตอบแบบคลิกเพื่อตั้งค่าจุดหยุดการทำงาน, เดินผ่านโค้ด, และการติดตามตัวแปร

การทำความเข้าใจความสามารถของตัวดีบักเกอร์ของคุณ เช่น จุดหยุดการทำงานตามเงื่อนไข, จุดที่ต้องจับตาดู, หรือการประเมินนิพจน์, สามารถเพิ่มประสิทธิภาพของคุณอย่างมากในการวินิจฉัยปัญหา

## ดูเพิ่มเติม
- [เอกสาร GDB](https://www.gnu.org/software/gdb/documentation/)
- [เอกสารคำสั่ง LLDB](https://lldb.llvm.org/use/map.html)
- [การสอนการใช้ตัวดีบักเกอร์ของ Visual Studio](https://docs.microsoft.com/en-us/visualstudio/debugger/debugger-feature-tour)
- [การดีบักด้วย CLion](https://www.jetbrains.com/help/clion/debugging-code.html)
