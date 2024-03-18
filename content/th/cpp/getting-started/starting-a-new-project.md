---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:51:15.257118-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E42\
  \u0E1B\u0E23\u0E40\u0E08\u0E04\u0E43\u0E2B\u0E21\u0E48\u0E2B\u0E21\u0E32\u0E22\u0E16\
  \u0E36\u0E07\u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32\u0E1E\u0E37\
  \u0E49\u0E19\u0E10\u0E32\u0E19\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E10\u0E32\u0E19\
  \u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13 \u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E2A\u0E34\u0E48\u0E07\
  \u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E23\u0E30\u0E15\u0E38\u0E49\
  \u0E19\u0E01\u0E23\u0E30\u0E1A\u0E27\u0E19\u0E01\u0E32\u0E23\u0E1E\u0E31\u0E12\u0E19\
  \u0E32, \u0E41\u0E19\u0E30\u0E19\u0E33\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\
  \u0E32\u0E07\u0E02\u0E2D\u0E07\u0E42\u0E1B\u0E23\u0E40\u0E08\u0E04\u2026"
lastmod: '2024-03-17T21:57:56.522238-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E42\
  \u0E1B\u0E23\u0E40\u0E08\u0E04\u0E43\u0E2B\u0E21\u0E48\u0E2B\u0E21\u0E32\u0E22\u0E16\
  \u0E36\u0E07\u0E01\u0E32\u0E23\u0E15\u0E31\u0E49\u0E07\u0E04\u0E48\u0E32\u0E1E\u0E37\
  \u0E49\u0E19\u0E10\u0E32\u0E19\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E10\u0E32\u0E19\
  \u0E42\u0E04\u0E49\u0E14\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13 \u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E2A\u0E34\u0E48\u0E07\
  \u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E23\u0E30\u0E15\u0E38\u0E49\
  \u0E19\u0E01\u0E23\u0E30\u0E1A\u0E27\u0E19\u0E01\u0E32\u0E23\u0E1E\u0E31\u0E12\u0E19\
  \u0E32, \u0E41\u0E19\u0E30\u0E19\u0E33\u0E42\u0E04\u0E23\u0E07\u0E2A\u0E23\u0E49\
  \u0E32\u0E07\u0E02\u0E2D\u0E07\u0E42\u0E1B\u0E23\u0E40\u0E08\u0E04\u2026"
title: "\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E42\u0E04\u0E23\u0E07\u0E01\
  \u0E32\u0E23\u0E43\u0E2B\u0E21\u0E48"
---

{{< edit_this_page >}}

## คืออะไร & ทำไม?
การเริ่มต้นโปรเจคใหม่หมายถึงการตั้งค่าพื้นฐานสำหรับฐานโค้ดของคุณ โปรแกรมเมอร์ทำสิ่งนี้เพื่อกระตุ้นกระบวนการพัฒนา, แนะนำโครงสร้างของโปรเจค และวางรากฐานสำหรับโค้ดในอนาคต

## วิธีทำ:
เมื่อเริ่มต้น, เลือกระบบสร้างหรือ IDE ของคุณ โดยเราจะใช้ตัวแก้ไขข้อความพื้นฐานและ g++ สำหรับความง่ายดาย สร้างสองไฟล์: `main.cpp` และ `Makefile`

`main.cpp`:
```C++
#include <iostream>

int main() {
    std::cout << "Hello, new project!" << std::endl;
    return 0;
}
```

`Makefile`:
```make
all:
    g++ main.cpp -o my_project

clean:
    rm my_project
```

เพื่อคอมไพล์, ทำการรัน `make` ในเทอร์มินัล เพื่อทำความสะอาด, ทำการรัน `make clean`.

ผลลัพธ์หลังจากรัน `./my_project`:
```
Hello, new project!
```

## ลงลึก
ในอดีต, การตั้งค่าโปรเจค C++ ใหม่เป็นกระบวนการที่ต้องทำด้วยมือมากขึ้น ในปัจจุบัน, IDEs สามารถสร้างเทมเพลตได้ ตัวเลือกเช่น CMake หรือ Meson ช่วยจัดการการสร้าง ก่อนมีเครื่องมือพวกนี้, นักพัฒนาเขียน Makefiles ด้วยมือ, คอมไพล์ทุกไฟล์ `.cpp` เป็นไฟล์วัตถุก่อนจะลิงค์พวกมัน

การพิจารณาทางเลือก: ระบบสร้างใหม่ทำให้กระบวนการง่ายขึ้น ตัวอย่างเช่น, CMake สร้าง Makefiles ของคุณแบบอัตโนมัติ, ทำให้มันเป็นอิสระต่อแพลตฟอร์ม

ในแง่ของการทำงาน, การตั้งค่าขึ้นอยู่กับปัจจัยเช่นขนาดของโปรเจคและการพึ่งพา โปรเจคใหญ่ต้องการโครงสร้างที่ซับซ้อนมากขึ้นด้วยโฟลเดอร์แยกสำหรับไฟล์ต้นฉบับ, ส่วนหัว, และการทดสอบ

## ดูเพิ่มเติม
- [เอกสาร CMake](https://cmake.org/documentation/)
- [แนวทางหลัก C++](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines)
- [GCC, the GNU Compiler Collection](https://gcc.gnu.org/)
