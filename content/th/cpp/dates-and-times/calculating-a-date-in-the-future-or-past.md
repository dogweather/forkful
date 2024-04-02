---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:01.201184-06:00
description: "\u0E01\u0E32\u0E23\u0E04\u0E33\u0E19\u0E27\u0E13\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48\u0E43\u0E19\u0E2D\u0E19\u0E32\u0E04\u0E15\u0E2B\u0E23\u0E37\u0E2D\u0E2D\
  \u0E14\u0E35\u0E15\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E04\
  \u0E49\u0E19\u0E2B\u0E32\u0E27\u0E48\u0E32\u0E2B\u0E25\u0E31\u0E07\u0E08\u0E32\u0E01\
  \u0E2B\u0E23\u0E37\u0E2D\u0E01\u0E48\u0E2D\u0E19\u0E0A\u0E48\u0E27\u0E07\u0E40\u0E27\
  \u0E25\u0E32\u0E2B\u0E19\u0E36\u0E48\u0E07 \u0E46 \u0E41\u0E25\u0E49\u0E27\u0E27\
  \u0E31\u0E19\u0E17\u0E35\u0E48\u0E08\u0E30\u0E40\u0E1B\u0E47\u0E19\u0E2D\u0E22\u0E48\
  \u0E32\u0E07\u0E44\u0E23 \u0E21\u0E31\u0E19\u0E21\u0E35\u0E1B\u0E23\u0E30\u0E42\u0E22\
  \u0E0A\u0E19\u0E4C\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E01\u0E32\u0E23\u0E41\u0E08\u0E49\u0E07\u0E40\u0E15\u0E37\u0E2D\
  \u0E19,\u2026"
lastmod: '2024-03-17T21:57:56.534487-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E04\u0E33\u0E19\u0E27\u0E13\u0E27\u0E31\u0E19\u0E17\
  \u0E35\u0E48\u0E43\u0E19\u0E2D\u0E19\u0E32\u0E04\u0E15\u0E2B\u0E23\u0E37\u0E2D\u0E2D\
  \u0E14\u0E35\u0E15\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E04\
  \u0E49\u0E19\u0E2B\u0E32\u0E27\u0E48\u0E32\u0E2B\u0E25\u0E31\u0E07\u0E08\u0E32\u0E01\
  \u0E2B\u0E23\u0E37\u0E2D\u0E01\u0E48\u0E2D\u0E19\u0E0A\u0E48\u0E27\u0E07\u0E40\u0E27\
  \u0E25\u0E32\u0E2B\u0E19\u0E36\u0E48\u0E07 \u0E46 \u0E41\u0E25\u0E49\u0E27\u0E27\
  \u0E31\u0E19\u0E17\u0E35\u0E48\u0E08\u0E30\u0E40\u0E1B\u0E47\u0E19\u0E2D\u0E22\u0E48\
  \u0E32\u0E07\u0E44\u0E23 \u0E21\u0E31\u0E19\u0E21\u0E35\u0E1B\u0E23\u0E30\u0E42\u0E22\
  \u0E0A\u0E19\u0E4C\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E2A\u0E23\
  \u0E49\u0E32\u0E07\u0E01\u0E32\u0E23\u0E41\u0E08\u0E49\u0E07\u0E40\u0E15\u0E37\u0E2D\
  \u0E19,\u2026"
title: "\u0E01\u0E32\u0E23\u0E04\u0E33\u0E19\u0E27\u0E13\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E43\u0E19\u0E2D\u0E19\u0E32\u0E04\u0E15\u0E2B\u0E23\u0E37\u0E2D\u0E2D\u0E14\
  \u0E35\u0E15"
weight: 26
---

## อะไรและทำไม?
การคำนวณวันที่ในอนาคตหรืออดีตหมายถึงการค้นหาว่าหลังจากหรือก่อนช่วงเวลาหนึ่ง ๆ แล้ววันที่จะเป็นอย่างไร มันมีประโยชน์สำหรับการสร้างการแจ้งเตือน, การตั้งวันหมดอายุ, การกำหนดการงาน, หรือแค่การบันทึกว่าเวลาผ่านไปมากแค่ไหน

## วิธีการ:
C++20 ได้แนะนำการอัพเกรดไลบรารี `<chrono>` จึงทำให้การจัดการกับเวลาง่ายขึ้น นี่คือตัวอย่างการเพิ่มวันให้กับวันที่ปัจจุบัน:

```C++
#include <iostream>
#include <chrono>
#include <format>

using namespace std::chrono;

int main() {
    // รับวันที่ของวันนี้
    auto today = floor<days>(system_clock::now());
    
    // เพิ่ม 30 วันให้กับวันนี้
    auto future_date = today + days(30);
    
    // แปลงเป็น time_point เพื่อแสดงผลโดยใช้ system_clock
    auto tp = system_clock::time_point(future_date);
    
    // แสดงผล
    std::cout << "วันที่ของวันนี้: "
              << std::format("{:%F}\n", today);
    std::cout << "วันที่ในอนาคต (30 วันถัดไป): "
              << std::format("{:%F}\n", tp);
    return 0;
}
```

ตัวอย่างผลลัพธ์:
```
วันที่ของวันนี้: 2023-03-15
วันที่ในอนาคต (30 วันถัดไป): 2023-04-14
```

การลบวันทำงานได้คล้ายกัน—คุณเพียงแค่ใช้ `-` แทน `+`.

## ศึกษาเพิ่มเติม
ก่อน C++20, คุณอาจจะใช้ไลบรารีเช่น Boost เพื่อจัดการกับวันที่ แต่ `<chrono>` ที่ได้รับการอัพเดตทำให้ง่ายขึ้นด้วย `system_clock`, `year_month_day`, และการเปลี่ยนแปลง `duration`.

จากประวัติศาสตร์, การคำนวณวันที่เป็นเรื่องซับซ้อนเนื่องจากต้องจัดการกับความยาวของเดือนที่แตกต่างกัน, ปีอกสัมพันธ์, และเขตเวลาด้วยตนเอง C++20 `<chrono>` จัดการกับเรื่องเหล่านี้โดยการให้การสนับสนุนปฏิทินและเขตเวลา

ทางเลือกอื่น? คุณยังสามารถใช้ Boost หรือแม้กระทั่งสร้างตรรกะวันที่ของคุณเอง (ผจญภัย, แต่ทำไม?) ยังมีไลบรารีของบุคคลที่สามเช่นไลบรารี "date" ของ Howard Hinnant, ซึ่งมีอิทธิพลต่อการอัพเดต chrono ใน C++20

ในแง่ของการเป็นต้นแบบ, `<chrono>` กำหนดระยะเวลาเป็นค่าคงที่แบบมีเหตุผลในช่วงเวลาคอมไพล์, หลีกเลี่ยงปัญหาจุดลอยตัว ประเภทเช่น `year_month_day` พึ่งพา `sys_days` ซึ่งเป็นการแสดง time_point เป็นวันนับจากสมัยกลาง (1970-01-01)

## ดูเพิ่มเติม
- การอ้างอิง C++ สำหรับ `chrono`: https://en.cppreference.com/w/cpp/header/chrono
- ห้องสมุดวันที่ของ Howard Hinnant (ก่อนการปรับปรุง chrono ของ C++20): https://github.com/HowardHinnant/date
- คู่มือ Boost Date/Time: https://www.boost.org/doc/libs/release/libs/date_time/
