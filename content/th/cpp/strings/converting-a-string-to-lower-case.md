---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:59.566845-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \u0E43\u0E2B\u0E49\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\
  \u0E4C\u0E40\u0E25\u0E47\u0E01\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\
  \u0E29\u0E23\u0E15\u0E31\u0E27\u0E43\u0E2B\u0E0D\u0E48\u0E17\u0E31\u0E49\u0E07\u0E2B\
  \u0E21\u0E14\u0E43\u0E2B\u0E49\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E2D\u0E31\
  \u0E01\u0E29\u0E23\u0E15\u0E31\u0E27\u0E40\u0E25\u0E47\u0E01\u0E17\u0E35\u0E48\u0E2A\
  \u0E2D\u0E14\u0E04\u0E25\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E19\u2026"
lastmod: '2024-03-17T21:57:56.509233-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \u0E43\u0E2B\u0E49\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\
  \u0E4C\u0E40\u0E25\u0E47\u0E01\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\
  \u0E23\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E15\u0E31\u0E27\u0E2D\u0E31\u0E01\
  \u0E29\u0E23\u0E15\u0E31\u0E27\u0E43\u0E2B\u0E0D\u0E48\u0E17\u0E31\u0E49\u0E07\u0E2B\
  \u0E21\u0E14\u0E43\u0E2B\u0E49\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E2D\u0E31\
  \u0E01\u0E29\u0E23\u0E15\u0E31\u0E27\u0E40\u0E25\u0E47\u0E01\u0E17\u0E35\u0E48\u0E2A\
  \u0E2D\u0E14\u0E04\u0E25\u0E49\u0E2D\u0E07\u0E01\u0E31\u0E19\u2026"
title: "\u0E41\u0E1B\u0E25\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\
  \u0E15\u0E31\u0E27\u0E40\u0E25\u0E47\u0E01"
weight: 4
---

## อะไรและทำไม?
การแปลงสตริงให้เป็นตัวพิมพ์เล็กหมายถึงการเปลี่ยนตัวอักษรตัวใหญ่ทั้งหมดให้เป็นตัวอักษรตัวเล็กที่สอดคล้องกัน โปรแกรมเมอร์ทำเช่นนี้เพื่อความสม่ำเสมอในการป้อนข้อมูลของผู้ใช้ การประมวลผลข้อมูล และเพื่อทำให้การเปรียบเทียบข้อความง่ายขึ้น

## วิธีทำ:
นี่คือวิธีที่คุณสามารถแปลงตัวอักษรตัวใหญ่ให้เป็นตัวอักษรตัวเล็กในภาษา C++:

```C++
#include <iostream>
#include <string>
#include <algorithm>

int main() {
    std::string origText = "C++ makes me Shout!";
    std::string lowerText = origText;

    std::transform(origText.begin(), origText.end(), lowerText.begin(), 
                   [](unsigned char c) { return std::tolower(c); });

    std::cout << "Original: " << origText << std::endl;
    std::cout << "Lowercase: " << lowerText << std::endl;
    
    return 0;
}
```
ผลลัพธ์:
```
Original: C++ makes me Shout!
Lowercase: c++ makes me shout!
```

## ศึกษาลึกซึ้ง
ในอดีต ก่อนที่ `std::transform` และ lambda จะถูกนำมาใช้ ใครๆ ต้องวนลูปผ่านแต่ละอักขระและเปลี่ยนเป็นตัวพิมพ์เล็กด้วยตัวเอง — ซึ่งต้องใช้แรงงานมากขึ้น `std::transform` ร่วมกับ `std::tolower` มีประสิทธิภาพและมีโอกาสผิดพลาดน้อยกว่า อย่างไรก็ตาม ทราบว่า C++ มีวิธีอื่นอยู่มาก Mind the locale: พฤติกรรมของ `std::tolower` อาจแตกต่างกันไป หากโปรเจคของคุณต้องการใช้ Unicode ลองดูไลบรารีของบุคคลที่สามเช่น ICU ที่ถูกสร้างขึ้นสำหรับเวทีโลก

นอกจากนี้ยังควรกล่าวถึงการเพิ่มเติมของ C++20, `std::ranges::transform`, ซึ่งนำเสนอการแปลงตามช่วงข้อมูล, ทำให้ไวยากรณ์มีรสนิยมมากขึ้นและปฏิบัติตามปรัชญา 'range' ที่การเขียนโค้ดควรจะง่ายต่อการเข้าใจและมีโอกาสผิดพลาดน้อยลง

ในส่วนของรายละเอียดการประยุกต์ใช้งาน แต่ละอักขระมีค่า ASCII และความแตกต่างระหว่างตัวพิมพ์เล็กและตัวพิมพ์ใหญ่นั้นคงที่ การเปลี่ยนแปลงจะตรวจสอบค่าเหล่านี้เพื่อลดลง—โดยพื้นฐานแล้วเล่นเกมลดค่าเลข

## ดูเพิ่มเติม
สำหรับคนที่คิดอยากทราบเพิ่มเติม:

- อ้างอิง C++ สำหรับ `std::transform`: https://en.cppreference.com/w/cpp/algorithm/transform
- อ้างอิง C++ สำหรับ `std::tolower`: https://en.cppreference.com/w/cpp/string/byte/tolower
- รายละเอียดเกี่ยวกับ C++20's `std::ranges`: https://en.cppreference.com/w/cpp/ranges

ต้องการเข้าใจเกี่ยวกับ Unicode มากขึ้น? ลองดูโปรเจกต์ ICU:
- โปรเจกต์ ICU: http://site.icu-project.org/home
