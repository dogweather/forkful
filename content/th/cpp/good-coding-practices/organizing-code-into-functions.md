---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:12.198054-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E25\u0E2D\u0E07\u0E17\
  \u0E33\u0E07\u0E32\u0E19\u0E17\u0E31\u0E48\u0E27\u0E44\u0E1B: \u0E01\u0E32\u0E23\
  \u0E04\u0E33\u0E19\u0E27\u0E13\u0E1E\u0E37\u0E49\u0E19\u0E17\u0E35\u0E48\u0E02\u0E2D\
  \u0E07\u0E27\u0E07\u0E01\u0E25\u0E21 \u0E41\u0E17\u0E19\u0E17\u0E35\u0E48\u0E08\u0E30\
  \u0E40\u0E02\u0E35\u0E22\u0E19\u0E2A\u0E39\u0E15\u0E23\u0E40\u0E14\u0E35\u0E22\u0E27\
  \u0E01\u0E31\u0E19\u0E17\u0E38\u0E01\u0E04\u0E23\u0E31\u0E49\u0E07 \u0E40\u0E23\u0E32\
  \ encapsulate \u0E21\u0E31\u0E19\u0E44\u0E27\u0E49\u0E43\u0E19\u0E1F\u0E31\u0E07\
  \u0E01\u0E4C\u0E0A\u0E31\u0E19."
lastmod: '2024-04-05T21:54:02.403810-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E23\u0E30\u0E40\u0E1A\u0E35\u0E22\u0E1A\
  \u0E42\u0E04\u0E49\u0E14\u0E40\u0E02\u0E49\u0E32\u0E44\u0E1B\u0E43\u0E19\u0E1F\u0E31\
  \u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19"
weight: 18
---

## วิธีการ:
ลองทำงานทั่วไป: การคำนวณพื้นที่ของวงกลม แทนที่จะเขียนสูตรเดียวกันทุกครั้ง เรา encapsulate มันไว้ในฟังก์ชัน

```C++
#include <iostream>
#define PI 3.14159

double calculateCircleArea(double radius) {
    return PI * radius * radius;
}

int main() {
    double r = 5.0;
    std::cout << "พื้นที่ของวงกลมที่มีรัศมี " << r << " คือ " << calculateCircleArea(r) << std::endl;
    return 0;
}
```

ผลลัพธ์ตัวอย่าง:
```
พื้นที่ของวงกลมที่มีรัศมี 5 คือ 78.5397
```

## ลึกลงไป
ในอดีต ขั้นตอนการทำงานและฟังก์ชันเป็นหลักสูตรของการเขียนโครงสร้างโปรแกรม ซึ่งได้รับการสนับสนุนในทศวรรษที่ 1960 เพื่อต่อสู้กับปัญหาของ "โค้ดสปาเก็ตตี้" ในภาษาโปรแกรมประเภท imperative ในยุคแรกๆ ทางเลือกเช่น OOP (Object-Oriented Programming) พัฒนาไปยังขั้นตอนถัดไปด้วยการเชื่อมโยงฟังก์ชันเหล่านี้กับโครงสร้างข้อมูล ใน C++ คุณมีฟังก์ชันปกติ, วิธีการของคลาส (รวมถึงเมธอดสถิต), lambda และฟังก์ชันแม่แบบ, แต่ละอันนำเสนอประโยชน์ที่แตกต่างกัน การติดตั้งฟังก์ชันที่จัดระเบียบได้ดีมักเกี่ยวข้องกับการปฏิบัติตามหลักการเช่น DRY ("Don't Repeat Yourself") และ SRP (Single Responsibility Principle) ซึ่งหมายความว่าแต่ละฟังก์ชันทำเพียงหนึ่งอย่างเท่านั้นและทำให้ดีที่สุด

## ดูเพิ่มเติม
สำหรับข้อมูลเพิ่มเติมเกี่ยวกับฟังก์ชันใน C++:
- https://en.cppreference.com/w/cpp/language/functions
- https://www.learncpp.com/cpp-tutorial/77-introduction-to-functions/

สำหรับหลักการออกแบบที่เกี่ยวข้องกับฟังก์ชัน:
- https://en.wikipedia.org/wiki/Single-responsibility_principle
- https://en.wikipedia.org/wiki/Don%27t_repeat_yourself

เรียนรู้เกี่ยวกับ lambdas และการใช้อย่างฟังก์ชันขั้นสูง:
- https://www.cprogramming.com/c++11/c++11-lambda-closures.html
- https://isocpp.org/wiki/faq/cpp14-language#lambda-captures
