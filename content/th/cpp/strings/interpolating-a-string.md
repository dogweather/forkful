---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:40.731917-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E15\u0E31\u0E27\u0E41\u0E1B\
  \u0E23\u0E25\u0E07\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07 \u0E2B\u0E21\u0E32\u0E22\
  \u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E43\u0E2A\u0E48\u0E15\u0E31\u0E27\u0E41\u0E1B\
  \u0E23\u0E25\u0E07\u0E44\u0E1B\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07 \u0E01\u0E32\
  \u0E23\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E1A\
  \u0E19\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E17\u0E35\u0E48\u0E40\u0E1B\u0E25\
  \u0E35\u0E48\u0E22\u0E19\u0E41\u0E1B\u0E25\u0E07\u0E44\u0E14\u0E49, \u0E01\u0E32\
  \u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E41\u0E15\u0E48\u0E07\u0E1C\u0E25\u0E25\u0E31\u0E1E\
  \u0E18\u0E4C\u0E43\u0E2B\u0E49\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E48\u0E27\u0E19\u0E15\
  \u0E31\u0E27,\u2026"
lastmod: '2024-03-17T21:57:56.508283-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E15\u0E31\u0E27\u0E41\u0E1B\
  \u0E23\u0E25\u0E07\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07 \u0E2B\u0E21\u0E32\u0E22\
  \u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E43\u0E2A\u0E48\u0E15\u0E31\u0E27\u0E41\u0E1B\
  \u0E23\u0E25\u0E07\u0E44\u0E1B\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07 \u0E01\u0E32\
  \u0E23\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\
  \u0E2D\u0E2A\u0E23\u0E49\u0E32\u0E07\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E1A\
  \u0E19\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E17\u0E35\u0E48\u0E40\u0E1B\u0E25\
  \u0E35\u0E48\u0E22\u0E19\u0E41\u0E1B\u0E25\u0E07\u0E44\u0E14\u0E49, \u0E01\u0E32\
  \u0E23\u0E1B\u0E23\u0E31\u0E1A\u0E41\u0E15\u0E48\u0E07\u0E1C\u0E25\u0E25\u0E31\u0E1E\
  \u0E18\u0E4C\u0E43\u0E2B\u0E49\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E48\u0E27\u0E19\u0E15\
  \u0E31\u0E27,\u2026"
title: "\u0E01\u0E32\u0E23\u0E41\u0E17\u0E23\u0E01\u0E04\u0E48\u0E32\u0E25\u0E07\u0E43\
  \u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
---

{{< edit_this_page >}}

## อะไรและทำไม?
การแทรกตัวแปรลงในสตริง หมายถึงการใส่ตัวแปรลงไปในสตริง การทำเช่นนี้เพื่อสร้างข้อความบนพื้นฐานที่เปลี่ยนแปลงได้, การปรับแต่งผลลัพธ์ให้เป็นส่วนตัว, หรือการสร้างคิวรีแบบไดนามิก

## วิธีการ:
C++ ไม่มีฟังก์ชันการแทรกสตริงภายในตัวอย่างบางภาษาอื่น ๆ คุณมักจะใช้ `std::ostringstream`, `std::format` (จาก C++20), หรือการจัดรูปแบบตามสไตล์ printf

ด้วย `std::ostringstream`:
```cpp
#include <sstream>
#include <iostream>

int main() {
    std::ostringstream message;
    int age = 30;
    message << "สวัสดี, ฉันอายุ " << age << " ปี.";
    std::cout << message.str() << std::endl; // "สวัสดี, ฉันอายุ 30 ปี."
}
```

ด้วย `std::format` (C++20):
```cpp
#include <format>
#include <iostream>

int main() {
    int age = 30;
    std::string message = std::format("สวัสดี, ฉันอายุ {} ปี.", age);
    std::cout << message << std::endl; // "สวัสดี, ฉันอายุ 30 ปี."
}
```

## ลงลึก
ก่อน C++20, เราต่อสตริงด้วยสตรีมหรือ sprintf ซึ่งไม่ค่อยสะดวก เมื่อมีการนำ `std::format` เข้ามา, เราเริ่มทันสมัยขึ้นเหมือนกับภาษาอื่น ๆ เช่น Python ที่มี f-strings

`std::ostringstream`: มันให้เราวิธีแบบสตรีมในการสร้างสตริงขึ้นมา มันหลากหลายแต่อาจไม่ใช่วิธีที่กระชับที่สุด มันเป็นที่นิยมใช้มาหลายปีเพราะมันปลอดภัยและใช้งานง่าย

`std::format`: ถูกนำมาใช้ใน C++20, มันเสนอการจัดรูปแบบคล้ายกับ Python มันอ่านง่ายและมีประสิทธิภาพมากกว่าการต่อสตรีม แต่ต้องการคอมไพเลอร์รุ่นใหม่

มีทางเลือกอื่น ๆ เช่น Boost.Format หรือการใช้การต่อสตริง, แต่พวกมันอาจไม่คล่องตัวเท่าหรืออาจทำให้เกิด overhead

การแทรกสตริงเป็นเพียงความสะดวก, แต่มันมีประโยชน์ มันทำให้โค้ดง่ายขึ้นและหลีกเลี่ยงการลดประสิทธิภาพจากการต่อเติมสตริงอย่างต่อเนื่อง

## ดูเพิ่มเติม
- [cppreference เกี่ยวกับ std::format](https://en.cppreference.com/w/cpp/utility/format)
- [cppreference เกี่ยวกับ std::ostringstream](https://en.cppreference.com/w/cpp/io/basic_ostringstream)
- [ห้องสมุด Boost.Format](https://www.boost.org/doc/libs/release/libs/format/)
