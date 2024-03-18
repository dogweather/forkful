---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:17.266022-06:00
description: "\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E01\
  \u0E32\u0E23\u0E2A\u0E19\u0E17\u0E19\u0E32\u0E01\u0E31\u0E1A\u0E42\u0E04\u0E49\u0E14\
  \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13; \u0E04\u0E38\u0E13\u0E41\u0E17\u0E23\u0E01\
  \u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E2A\u0E38\u0E02\u0E20\u0E32\
  \u0E1E\u0E41\u0E25\u0E30\u0E04\u0E27\u0E32\u0E21\u0E04\u0E34\u0E14\u0E02\u0E2D\u0E07\
  \u0E21\u0E31\u0E19\u2026"
lastmod: '2024-03-17T21:57:56.524062-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25\u0E14\u0E35\u0E1A\u0E31\u0E01\u0E40\u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E01\
  \u0E32\u0E23\u0E2A\u0E19\u0E17\u0E19\u0E32\u0E01\u0E31\u0E1A\u0E42\u0E04\u0E49\u0E14\
  \u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13; \u0E04\u0E38\u0E13\u0E41\u0E17\u0E23\u0E01\
  \u0E04\u0E33\u0E2A\u0E31\u0E48\u0E07\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E2A\u0E38\u0E02\u0E20\u0E32\
  \u0E1E\u0E41\u0E25\u0E30\u0E04\u0E27\u0E32\u0E21\u0E04\u0E34\u0E14\u0E02\u0E2D\u0E07\
  \u0E21\u0E31\u0E19\u2026"
title: "\u0E01\u0E32\u0E23\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E1C\u0E25\u0E25\u0E31\u0E1E\
  \u0E18\u0E4C\u0E01\u0E32\u0E23\u0E41\u0E01\u0E49\u0E44\u0E02\u0E42\u0E04\u0E49\u0E14"
---

{{< edit_this_page >}}

## อะไร & ทำไม?
การพิมพ์ข้อมูลดีบักเหมือนการสนทนากับโค้ดของคุณ; คุณแทรกคำสั่งพิมพ์เพื่อตรวจสอบสุขภาพและความคิดของมัน โปรแกรมเมอร์ทำเช่นนี้เพื่อติดตามหาข้อผิดพลาดหรือให้แน่ใจว่าทุกอย่างทำงานอย่างราบรื่น—เหมือนการให้การตรวจเช็คที่รวดเร็วกับโค้ดของคุณ

## วิธีการ:
นี่คือตัวอย่างส่วนของโค้ดที่แสดงวิธีการพิมพ์ข้อความดีบักง่ายๆ ไปยังคอนโซล

```C++
#include <iostream>

int main() {
    int lifeTheUniverseAndEverything = 42;

    // ข้อความดีบัก
    std::cout << "Debug: The value of lifeTheUniverseAndEverything is " 
              << lifeTheUniverseAndEverything << std::endl;

    // ส่วนที่เหลือของโค้ดอยู่ที่นี่...

    return 0;
}
```

ผลลัพธ์ตัวอย่าง:
```
Debug: The value of lifeTheUniverseAndEverything is 42
```

## ศึกษาลึกเพิ่มเติม
ในอดีต, ข้อมูลดีบักถูกแกะสลักบนสื่อทางกายภาพ ไม่สนุกเลย ตอนนี้, เราเพียงแค่ใช้ `std::cout` และเครื่องมือที่คล้ายคลึงกัน `std::cerr` มีไว้สำหรับข้อผิดพลาด มักใช้ควบคู่กับ `std::cout` ทำไมต้องมีสองสตรีมที่แตกต่างกัน? เหมือนการมีห้องสนทนาที่แตกต่างกันสำหรับงานและเพื่อนๆ; มันช่วยให้การจัดการเป็นระเบียบ ไอดีอีที่หรูหราให้ตัวดีบักเกอร์ที่บูรณาการอยู่ในตัว, แต่บางครั้งคำสั่งพิมพ์ง่ายๆ ก็ทำงานได้ตามประสงค์โดยไม่ต้องยุ่งยาก ระวัง, การพิมพ์ที่ไม่จำเป็นสามารถทำให้การทำงานช้าลง; จินตนาการถึงบางคนที่บรรยายทุกขั้นตอนที่พวกเขาทำ จัดการให้เรียบร้อยเมื่อคุณทำเสร็จแล้ว

## ดูเพิ่มเติม
- [cppreference.com](https://en.cppreference.com/w/cpp/io/cout) – เพื่อการเรียนรู้อย่างลึกซึ้งเกี่ยวกับ `std::cout` และเพื่อนๆ
- [GNU Project Debugger (GDB)](https://www.gnu.org/software/gdb/) - เมื่อคุณพร้อมที่จะเคลื่อนย้ายจากการพิมพ์ไปสู่ตัวดีบักเกอร์ที่ครบถ้วน
- [Stack Overflow](https://stackoverflow.com/questions/tagged/c%2b%2b) – เพื่อดูปัญหาที่คนอื่นๆ พบและวิธีที่การดีบักด้วยการพิมพ์สามารถช่วยได้
