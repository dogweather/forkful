---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:38.839477-06:00
description: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E04\u0E48\u0E32\u0E22\u0E48\u0E2D\
  \u0E22\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E16\
  \u0E36\u0E07\u0E01\u0E32\u0E23\u0E04\u0E31\u0E14\u0E40\u0E2D\u0E32\u0E2A\u0E48\u0E27\
  \u0E19\u0E22\u0E48\u0E2D\u0E22\u0E46 \u0E2D\u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\
  \u0E23\u0E34\u0E07\u0E17\u0E35\u0E48\u0E43\u0E2B\u0E0D\u0E48\u0E01\u0E27\u0E48\u0E32\
  \ \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\
  \u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E41\
  \u0E22\u0E01, \u0E1B\u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25, \u0E2B\u0E23\u0E37\
  \u0E2D\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E20\u0E32\u0E22\u0E43\u0E19\u0E02\u0E49\
  \u0E2D\u0E04\u0E27\u0E32\u0E21, \u0E40\u0E0A\u0E48\u0E19\u2026"
lastmod: '2024-03-17T21:57:56.511051-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E04\u0E48\u0E32\u0E22\u0E48\u0E2D\
  \u0E22\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E16\
  \u0E36\u0E07\u0E01\u0E32\u0E23\u0E04\u0E31\u0E14\u0E40\u0E2D\u0E32\u0E2A\u0E48\u0E27\
  \u0E19\u0E22\u0E48\u0E2D\u0E22\u0E46 \u0E2D\u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\
  \u0E23\u0E34\u0E07\u0E17\u0E35\u0E48\u0E43\u0E2B\u0E0D\u0E48\u0E01\u0E27\u0E48\u0E32\
  \ \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\
  \u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E41\
  \u0E22\u0E01, \u0E1B\u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25, \u0E2B\u0E23\u0E37\
  \u0E2D\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E20\u0E32\u0E22\u0E43\u0E19\u0E02\u0E49\
  \u0E2D\u0E04\u0E27\u0E32\u0E21, \u0E40\u0E0A\u0E48\u0E19\u2026"
title: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E22\
  \u0E48\u0E2D\u0E22\u0E2D\u0E2D\u0E01\u0E21\u0E32"
---

{{< edit_this_page >}}

## อะไรและทำไม?

การดึงค่าย่อยจากสตริงหมายถึงการคัดเอาส่วนย่อยๆ ออกจากสตริงที่ใหญ่กว่า โปรแกรมเมอร์ทำเช่นนี้เพื่อแยก, ประมวลผล, หรือวิเคราะห์ข้อมูลเฉพาะภายในข้อความ, เช่น การดึงชื่อผู้ใช้งานออกจากที่อยู่อีเมล หรือวันที่จากบันทึกการล็อก

## วิธีการ:

C++ ทำให้การดึงค่าย่อยเป็นเรื่องง่าย `std::string` เป็นเพื่อนคู่ใจในที่นี้ ด้วยฟังก์ชัน `substr()` ที่ทำงานหนักส่วนใหญ่ มาดูโค้ดกันเลย:

```C++
#include <iostream>
#include <string>

int main() {
    std::string fullString = "Hello, World! Programming in C++ is fun.";
    std::string snippet;

    // ดึงค่า "World" โดยเริ่มที่ดัชนี 7 ด้วยความยาว 5
    snippet = fullString.substr(7, 5);
    std::cout << snippet << std::endl; // ผลลัพธ์: World

    // ดึงค่า "Programming" โดยเริ่มที่ดัชนี 14
    snippet = fullString.substr(14);
    std::cout << snippet << std::endl; // ผลลัพธ์: Programming in C++ is fun.

    return 0;
}
```

## ลงลึก

การดึงค่าย่อยไม่ใช่เรื่องใหม่ โปรแกรมเมอร์ C แบบเก่าใช้ `strncpy` และการบันทึกด้วยมือ การจัดการสตริงเป็นแหล่งที่มาของข้อผิดพลาดทั่วไป ดังนั้น C++ จึงมุ่งเน้นที่จะทำให้มันเรียบง่ายขึ้น `std::string` และเมทอด `substr` ของมันเริ่มต้นมาตั้งแต่ C++98 และได้ช่วยลดความเครียดมาตั้งแต่นั้น

ทางเลือก? แน่นอน คุณอาจจะลงมือทำเองด้วย `std::string::iterator` หรือกลับไปใช้ฟังก์ชั่นภาษา C ถ้าคุณชอบความตื่นเต้น เทคนิคที่ทันสมัยกว่าอาจจะเกี่ยวข้องกับ string_views สำหรับการเพียงแค่ดูโดยไม่แก้ไข

การใช้งานภายใน? ภายใต้ผิวหนัง, `substr` บ่อยครั้งที่จัดสรรพื้นที่เก็บข้อมูลใหม่และคัดลอกข้อมูล, ซึ่งไม่ใช่ฟรี มันเบากว่าการต่อสู้กับพอยน์เตอร์ดิบและอาร์เรย์ของ char จากสมัยก่อน, แต่มันก็ไม่ใช่ทันที

## ดูเพิ่มเติม

สำหรับข้อมูลเพิ่มเติมเกี่ยวกับ `std::string` และเพื่อนๆ:
- cppreference.com เรื่อง `std::string`: https://en.cppreference.com/w/cpp/string/basic_string
- ข้อมูลเพิ่มเติมเกี่ยวกับ `std::string_view`: https://en.cppreference.com/w/cpp/string/basic_string_view
- การจัดการสตริงแบบภาษา C (สำหรับการย้อนยุค): http://www.cplusplus.com/reference/cstring/
