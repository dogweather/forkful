---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:23.624264-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E43\u0E19 C++ \u0E08\
  \u0E30\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E27\u0E31\u0E15\u0E16\u0E38\u0E27\
  \u0E31\u0E19\u0E17\u0E35\u0E48\u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E17\u0E35\u0E48\u0E2D\u0E48\u0E32\
  \u0E19\u0E44\u0E14\u0E49\u2026"
lastmod: '2024-03-17T21:57:56.532703-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E43\u0E19 C++ \u0E08\
  \u0E30\u0E40\u0E1B\u0E25\u0E35\u0E48\u0E22\u0E19\u0E27\u0E31\u0E15\u0E16\u0E38\u0E27\
  \u0E31\u0E19\u0E17\u0E35\u0E48\u0E40\u0E1B\u0E47\u0E19\u0E23\u0E39\u0E1B\u0E41\u0E1A\
  \u0E1A\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E17\u0E35\u0E48\u0E2D\u0E48\u0E32\
  \u0E19\u0E44\u0E14\u0E49\u2026"
title: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
---

{{< edit_this_page >}}

## อะไร & ทำไม?
การแปลงวันที่เป็นสตริงใน C++ จะเปลี่ยนวัตถุวันที่เป็นรูปแบบข้อความที่อ่านได้ นี่เป็นสิ่งสำคัญสำหรับการแสดงวันที่ต่อผู้ใช้และการบันทึกเหตุการณ์ในแบบที่เป็นมิตรกับมนุษย์

## วิธีทำ:
ใน C++ สมัยใหม่, ไลบรารี `<chrono>` และ `<iomanip>` เป็นเพื่อนซี้สำหรับการดำเนินการวันเวลา นี่คือวิธีการรวดเร็วโดยใช้ `std::put_time`:

```cpp
#include <iostream>
#include <iomanip>
#include <chrono>
#include <sstream>

int main() {
    auto now = std::chrono::system_clock::now(); // รับเวลาปัจจุบัน
    auto time = std::chrono::system_clock::to_time_t(now); // แปลงเป็น time_t
    
    // แปลงเป็นโครงสร้าง tm เพื่อการจัดรูปแบบ
    std::tm tm = *std::localtime(&time);

    // สตรีมสตริงสำหรับออกผล
    std::stringstream ss;

    ss << std::put_time(&tm, "%Y-%m-%d %H:%M:%S"); // รูปแบบ: ปี-เดือน-วัน ชั่วโมง:นาที:วินาที

    std::string date_str = ss.str(); // แปลงเป็นสตริง

    std::cout << date_str << std::endl; // แสดงผลสตริงวันที่
    return 0;
}
```

ผลลัพธ์ตัวอย่าง (ขึ้นอยู่กับวันเวลาปัจจุบัน):
```
2023-03-15 14:25:30
```

## ลึกลงไป
ก่อนที่ `<chrono>` จะถูกนำมาใช้, โปรแกรมเมอร์ C++ มักต้องดิ้นรนกับการจัดการเวลาแบบ C ผ่าน `<ctime>` ซึ่งมีความน้อยชัดและมีโอกาสผิดพลาดมากขึ้นเนื่องจากการจัดการหน่วยความจำด้วยตนเองและความประหลาดที่เฉพาะตามแพลตฟอร์ม

ทางเลือกอื่น ๆ สำหรับ `std::put_time` รวมถึงการใช้ `strftime`, แต่นั่นเป็นสไตล์มากกว่า C. ไลบรารีของบุคคลที่สามเช่น Boost.Date_Time สามารถเสนอฟังก์ชั่นเพิ่มเติมในราคาที่ต้องเพิ่มการพึ่งพิง

รายละเอียดสำคัญในการประยุกต์ใช้คือการเข้าใจตัวกำหนดรูปแบบใน `std::put_time`, ซึ่งคล้ายกับที่ใช้ใน `strftime` คุณกำลังจับคู่ตัวยึดที่ว่างเปล่ากับส่วนประกอบของวันหรือเวลา - `%Y` สำหรับปีเต็ม, `%m` สำหรับเดือน, และอื่น ๆ

## ดูเพิ่มเติม
- [เอกสารของ `<chrono>`](https://en.cppreference.com/w/cpp/header/chrono)
- [เอกสารของ `<iomanip>`](https://en.cppreference.com/w/cpp/header/iomanip)
- [Boost.Date_Time](https://www.boost.org/doc/libs/release/libs/date_time/)
