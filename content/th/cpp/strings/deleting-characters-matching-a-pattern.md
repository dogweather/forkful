---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:08.582434-06:00
description: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E17\
  \u0E35\u0E48\u0E15\u0E23\u0E07\u0E01\u0E31\u0E1A\u0E41\u0E1A\u0E1A\u0E41\u0E1C\u0E19\
  \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E01\u0E33\u0E08\u0E31\
  \u0E14\u0E25\u0E33\u0E14\u0E31\u0E1A\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E2D\u0E2D\u0E01\
  \u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E2A\u0E34\u0E48\u0E07\u0E19\u0E35\
  \u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E17\u0E33\u0E04\u0E27\u0E32\
  \u0E21\u0E2A\u0E30\u0E2D\u0E32\u0E14\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25, \u0E01\
  \u0E32\u0E23\u0E08\u0E31\u0E14\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E02\u0E49\u0E2D\
  \u0E21\u0E39\u0E25,\u2026"
lastmod: '2024-03-17T21:57:56.506338-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E17\
  \u0E35\u0E48\u0E15\u0E23\u0E07\u0E01\u0E31\u0E1A\u0E41\u0E1A\u0E1A\u0E41\u0E1C\u0E19\
  \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E01\u0E33\u0E08\u0E31\
  \u0E14\u0E25\u0E33\u0E14\u0E31\u0E1A\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E2D\u0E2D\u0E01\
  \u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\
  \u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E2A\u0E34\u0E48\u0E07\u0E19\u0E35\
  \u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E17\u0E33\u0E04\u0E27\u0E32\
  \u0E21\u0E2A\u0E30\u0E2D\u0E32\u0E14\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25, \u0E01\
  \u0E32\u0E23\u0E08\u0E31\u0E14\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E02\u0E49\u0E2D\
  \u0E21\u0E39\u0E25, \u0E2B\u0E23\u0E37\u0E2D\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E15\
  \u0E2D\u0E1A\u0E2A\u0E19\u0E2D\u0E07\u0E01\u0E0E\u0E02\u0E2D\u0E07\u0E41\u0E2D\u0E1B\
  \u0E1E\u0E25\u0E34\u0E40\u0E04\u0E0A\u0E31\u0E19."
title: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\u0E29\u0E23\
  \u0E17\u0E35\u0E48\u0E15\u0E23\u0E07\u0E01\u0E31\u0E1A\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A"
weight: 5
---

## วิธีการ:
มาดึงอักขระออกโดยใช้ `erase` และ `remove_if` ควบคู่ไปกับนิพจน์ lambda นี่คือตัวอย่างที่เร็ว:

```cpp
#include <iostream>
#include <algorithm>
#include <string>

int main() {
    std::string data = "B4n4n4!";

    // ลบอักขระตัวเลขทั้งหมด
    data.erase(std::remove_if(data.begin(), data.end(), ::isdigit), data.end());
    
    std::cout << data << std::endl; // แสดงผล: Bnn!
    
    return 0;
}
```
ตัวอย่างผลลัพธ์:
```
Bnn!
```

## การทำความลึก
อัลกอริทึม `std::remove_if` จากหัวข้อ `<algorithm>` ไม่ได้จริงๆ ลดขนาดสตริง; มันจัดเรียงองค์ประกอบใหม่และส่งกลับตัวชี้ไปยังตอนท้ายตามหลักเกณฑ์ใหม่ วิธีการ `erase` ของคลาส `std::string` จะลบส่วนที่ไม่จำเป็นออกจากตอนท้าย ชุดคำสั่งนี้มาพร้อมกับ C++98 และยังคงเป็นที่นิยมและมีประสิทธิภาพ

ทางเลือกอื่น? สำหรับแบบแผนที่ซับซ้อน, regex (`<regex>`) เป็นเครื่องมือหลายประโยชน์ แต่มันเหมือนกับใช้ปืนใหญ่ยิงนกสำหรับงานเล็กๆ

รายละเอียดเพิ่มเติม? `std::remove_if` และอัลกอริทึมที่คล้ายกันมีพื้นฐานมาจากไอเทอเรเตอร์, ซึ่ง C++ ได้รับมาจาก Standard Template Library (STL) ในช่วงกลางยุค 90s พวกมันสนับสนุนการเขียนโปรแกรมแบบเจเนอริก, รับรองว่าโค้ดการตัดและเปลี่ยนของคุณทำงานได้กับสตริง, ลิสต์, อะไรต่อมิอะไรก็ตาม

## ดูเพิ่มเติม
- อ้างอิง C++ สำหรับ `std::remove_if`: https://en.cppreference.com/w/cpp/algorithm/remove
- อ้างอิง C++ สำหรับ `std::string::erase`: https://en.cppreference.com/w/cpp/string/basic_string/erase
- เพิ่มเติมเกี่ยวกับไอเทอเรเตอร์ใน C++: https://en.cppreference.com/w/cpp/iterator
- เมื่อไรควรใช้ `std::regex` สำหรับการจับคู่แบบแผน: https://en.cppreference.com/w/cpp/regex
