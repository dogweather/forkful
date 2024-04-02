---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:15.252555-06:00
description: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E1B\u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19\
  \u0E43\u0E19\u0E20\u0E32\u0E29\u0E32 C++ \u0E40\u0E1B\u0E47\u0E19\u0E07\u0E32\u0E19\
  \u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E42\
  \u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E17\u0E35\u0E48\u0E15\u0E49\u0E2D\u0E07\u0E01\
  \u0E32\u0E23\u0E1B\u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25\u0E2B\u0E23\u0E37\u0E2D\
  \u0E41\u0E2A\u0E14\u0E07\u0E1C\u0E25\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E15\u0E32\
  \u0E21\u0E19\u0E32\u0E2C\u0E34\u0E01\u0E32\u0E02\u0E2D\u0E07\u0E23\u0E30\u0E1A\u0E1A\
  \ \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E34\u0E48\u0E07\u0E2A\u0E33\u0E04\u0E31\u0E0D\
  \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\
  \u0E01,\u2026"
lastmod: '2024-03-17T21:57:56.531765-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\
  \u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E1B\u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19\
  \u0E43\u0E19\u0E20\u0E32\u0E29\u0E32 C++ \u0E40\u0E1B\u0E47\u0E19\u0E07\u0E32\u0E19\
  \u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E42\
  \u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E17\u0E35\u0E48\u0E15\u0E49\u0E2D\u0E07\u0E01\
  \u0E32\u0E23\u0E1B\u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25\u0E2B\u0E23\u0E37\u0E2D\
  \u0E41\u0E2A\u0E14\u0E07\u0E1C\u0E25\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E15\u0E32\
  \u0E21\u0E19\u0E32\u0E2C\u0E34\u0E01\u0E32\u0E02\u0E2D\u0E07\u0E23\u0E30\u0E1A\u0E1A\
  \ \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E34\u0E48\u0E07\u0E2A\u0E33\u0E04\u0E31\u0E0D\
  \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E1A\u0E31\u0E19\u0E17\u0E36\
  \u0E01,\u2026"
title: "\u0E01\u0E32\u0E23\u0E23\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E1B\
  \u0E31\u0E08\u0E08\u0E38\u0E1A\u0E31\u0E19"
weight: 29
---

## อะไร & ทำไม?
การดึงข้อมูลวันที่ปัจจุบันในภาษา C++ เป็นงานพื้นฐานสำหรับโปรแกรมที่ต้องการประมวลผลหรือแสดงผลวันที่ตามนาฬิกาของระบบ เป็นสิ่งสำคัญสำหรับการบันทึก, การติดตามเวลา, การกำหนดเวลางาน และฟังก์ชั่นอื่นๆ ที่พึ่งพาวันที่และเวลา

## วิธีการ:
C++ มีหลายวิธีในการรับวันที่ปัจจุบัน รวมถึงห้องสมุดมาตรฐาน C++ และห้องสมุดของบุคคลที่สามเช่น Boost ตัวอย่างต่อไปนี้แสดงวิธีการทำงานนี้

### การใช้ `<chrono>` (C++20 และใหม่กว่า)
C++20 ได้เพิ่มฟังก์ชั่นการณ์เพิ่มเติมในห้องสมุด `<chrono>` ทำให้ง่ายต่อการรับวันที่ปัจจุบัน:
```cpp
#include <iostream>
#include <chrono>
#include <format> // สำหรับ std::format (C++20)

int main() {
    auto current_time_point = std::chrono::system_clock::now(); // บันทึกเวลาปัจจุบัน
    auto current_time_t = std::chrono::system_clock::to_time_t(current_time_point); // แปลงเป็น time_t

    // จัดรูปแบบเวลาให้อ่านง่าย
    std::cout << "วันที่ปัจจุบัน: " << std::format("{:%Y-%m-%d}", std::chrono::system_clock::to_time_t(current_time_point)) << std::endl;

    return 0;
}
```
**ตัวอย่างผลลัพธ์:**
```plaintext
วันที่ปัจจุบัน: 2023-03-15
```

### การใช้ `<ctime>`
สำหรับโปรแกรมเมอร์ที่ทำงานกับเวอร์ชันเก่าของ C++ หรือผู้ที่ชอบห้องสมุด C แบบดั้งเดิม:
```cpp
#include <iostream>
#include <ctime>

int main() {
    std::time_t t = std::time(0); // รับเวลาปัจจุบัน
    std::tm* now = std::localtime(&t);
    std::cout << "วันที่ปัจจุบัน: "
              << (now->tm_year + 1900) << '-'
              << (now->tm_mon + 1) << '-'
              <<  now->tm_mday
              << std::endl;

    return 0;
}
```
**ตัวอย่างผลลัพธ์:**
```plaintext
วันที่ปัจจุบัน: 2023-03-15
```

### การใช้ Boost Date_Time
สำหรับโครงการที่ใช้ห้องสมุด Boost, ห้องสมุด Boost Date_Time มีวิธีการแทนที่ในการรับวันที่ปัจจุบัน:
```cpp
#include <iostream>
#include <boost/date_time.hpp>

int main() {
    // รับวันปัจจุบันโดยใช้ปฏิทิน Gregorian ของ Boost
    boost::gregorian::date today = boost::gregorian::day_clock::local_day();
    std::cout << "วันที่ปัจจุบัน: " << today << std::endl;

    return 0;
}
```
**ตัวอย่างผลลัพธ์:**
```plaintext
วันที่ปัจจุบัน: 2023-Mar-15
```
ตัวอย่างเหล่านี้เป็นพื้นฐานสำคัญสำหรับการทำงานกับวันที่ใน C++, ซึ่งสำคัญสำหรับแอปพลิเคชั่นหลายๆ ประเภท.
