---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:52:24.158114-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: C++11 \u0E44\u0E14\u0E49\
  \u0E40\u0E1E\u0E34\u0E48\u0E21\u0E01\u0E32\u0E23\u0E2A\u0E19\u0E31\u0E1A\u0E2A\u0E19\
  \u0E38\u0E19\u0E19\u0E34\u0E1E\u0E08\u0E19\u0E4C\u0E1B\u0E23\u0E01\u0E15\u0E34\u0E43\
  \u0E19\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\
  \u0E19, `<regex>`, \u0E42\u0E14\u0E22\u0E19\u0E33\u0E40\u0E2A\u0E19\u0E2D\u0E01\u0E23\
  \u0E2D\u0E1A\u0E07\u0E32\u0E19\u0E17\u0E35\u0E48\u0E21\u0E31\u0E48\u0E19\u0E04\u0E07\
  \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\
  \u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E2A\u0E15\
  \u0E23\u0E34\u0E07\u2026"
lastmod: '2024-03-17T21:57:56.512012-06:00'
model: gpt-4-0125-preview
summary: "C++11 \u0E44\u0E14\u0E49\u0E40\u0E1E\u0E34\u0E48\u0E21\u0E01\u0E32\u0E23\
  \u0E2A\u0E19\u0E31\u0E1A\u0E2A\u0E19\u0E38\u0E19\u0E19\u0E34\u0E1E\u0E08\u0E19\u0E4C\
  \u0E1B\u0E23\u0E01\u0E15\u0E34\u0E43\u0E19\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\
  \u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\u0E19, `<regex>`, \u0E42\u0E14\u0E22\u0E19\u0E33\
  \u0E40\u0E2A\u0E19\u0E2D\u0E01\u0E23\u0E2D\u0E1A\u0E07\u0E32\u0E19\u0E17\u0E35\u0E48\
  \u0E21\u0E31\u0E48\u0E19\u0E04\u0E07\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\
  \u0E23\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E41\u0E25\u0E30\u0E01\u0E32\u0E23\u0E08\u0E31\
  \u0E14\u0E01\u0E32\u0E23\u0E2A\u0E15\u0E23\u0E34\u0E07 \u0E19\u0E35\u0E48\u0E04\u0E37\
  \u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E1E\u0E37\u0E49\u0E19\u0E10\
  \u0E32\u0E19\u0E02\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E19\u0E34\u0E1E\
  \u0E08\u0E19\u0E4C\u0E1B\u0E23\u0E01\u0E15\u0E34\u0E43\u0E19\u0E01\u0E32\u0E23\u0E04\
  \u0E49\u0E19\u0E2B\u0E32\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E20\u0E32\u0E22\u0E43\
  \u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07."
title: "\u0E01\u0E32\u0E23\u0E43\u0E0A\u0E49\u0E40\u0E23\u0E01\u0E38\u0E25\u0E32\u0E23\
  \u0E4C\u0E40\u0E2D\u0E47\u0E01\u0E40\u0E1E\u0E23\u0E2A\u0E0A\u0E31\u0E19"
weight: 11
---

## วิธีการ:
C++11 ได้เพิ่มการสนับสนุนนิพจน์ปรกติในไลบรารีมาตรฐาน, `<regex>`, โดยนำเสนอกรอบงานที่มั่นคงสำหรับการค้นหาและการจัดการสตริง นี่คือตัวอย่างพื้นฐานของการใช้นิพจน์ปรกติในการค้นหารูปแบบภายในสตริง:

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string target = "Hello, my email is example@example.com";
    std::regex email_pattern(R"(\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b)");

    if (std::regex_search(target, email_pattern)) {
        std::cout << "พบอีเมล!" << std::endl;
    } else {
        std::cout << "ไม่พบอีเมล." << std::endl;
    }

    return 0;
}
```
**ผลลัพธ์ตัวอย่าง**
```
พบอีเมล!
```

สำหรับการจัดการที่ซับซ้อนมากขึ้น เช่น การแทนที่รูปแบบภายในสตริง นิพจน์ปรกติของ C++ สามารถใช้ได้ประโยชน์มาก:

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string text = "The rain in Spain falls mainly in the plain.";
    std::regex vowel_regex("([aeiou])");

    std::string replaced_text = std::regex_replace(text, vowel_regex, "*");
    std::cout << replaced_text << std::endl;

    return 0;
}
```
**ผลลัพธ์ตัวอย่าง**
```
Th* r**n *n Sp**n f*lls m**nly *n th* pl**n.
```

สำหรับโปรแกรมเมอร์ที่สำรวจนอกไลบรารีมาตรฐาน เลือกใช้ไลบรารี Boost Regex (`boost/regex.hpp`) เป็นทางเลือกยอดนิยมจากบุคคลที่สามที่มอบความสามารถรูปแบบปรกติที่เพิ่มขึ้นและการปรับให้เหมาะสมด้านประสิทธิภาพ โดยเฉพาะสำหรับรูปแบบที่ซับซ้อนหรือการประมวลผลข้อมูลขนาดใหญ่:

```cpp
#include <iostream>
#include <boost/regex.hpp>

int main() {
    std::string s = "Boost libraries are fun!";
    boost::regex expr("(\\w+)\\s(libraries)"); // จับคู่ "Boost libraries"
    std::string fmt("GNU \\1"); // แทนที่ด้วย "GNU Boost"

    std::string result = boost::regex_replace(s, expr, fmt);
    std::cout << result << std::endl;

    return 0;
}
```
**ผลลัพธ์ตัวอย่าง**
```
GNU Boost สนุก!
```

ตัวอย่างเหล่านี้เป็นการนำเสนอความสามารถของ C++ ในการใช้นิพจน์ปรกติเพียงผิวเผิน โดยแสดงการค้นหาพื้นฐาน, การจับคู่รูปแบบ และการแทนที่ ไม่ว่าจะใช้ไลบรารีมาตรฐานหรือได้รับการเสริมความสามารถด้วยไลบรารี regex อันทรงพลังของ Boost.
