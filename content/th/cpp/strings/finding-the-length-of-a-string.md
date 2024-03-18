---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:02.352522-06:00
description: "\u0E01\u0E32\u0E23\u0E2B\u0E32\u0E04\u0E27\u0E32\u0E21\u0E22\u0E32\u0E27\
  \u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E27\u0E48\u0E32\u0E21\u0E35\
  \u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\
  \u0E17\u0E48\u0E32\u0E44\u0E2B\u0E23\u0E48 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\
  \u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E2A\u0E34\u0E48\u0E07\u0E19\u0E35\u0E49\
  \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E1B\u0E49\u0E2D\u0E19, \u0E15\u0E31\
  \u0E49\u0E07\u0E04\u0E48\u0E32\u0E25\u0E39\u0E1B, \u0E2B\u0E23\u0E37\u0E2D\u0E40\
  \u0E15\u0E23\u0E35\u0E22\u0E21\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u2026"
lastmod: '2024-03-17T21:57:56.512964-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2B\u0E32\u0E04\u0E27\u0E32\u0E21\u0E22\u0E32\u0E27\
  \u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E27\u0E48\u0E32\u0E21\u0E35\
  \u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\
  \u0E17\u0E48\u0E32\u0E44\u0E2B\u0E23\u0E48 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\
  \u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E2A\u0E34\u0E48\u0E07\u0E19\u0E35\u0E49\
  \u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\u0E02\u0E49\
  \u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E1B\u0E49\u0E2D\u0E19, \u0E15\u0E31\
  \u0E49\u0E07\u0E04\u0E48\u0E32\u0E25\u0E39\u0E1B, \u0E2B\u0E23\u0E37\u0E2D\u0E40\
  \u0E15\u0E23\u0E35\u0E22\u0E21\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E2A\u0E33\u0E2B\
  \u0E23\u0E31\u0E1A\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19\u2026"
title: "\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E04\u0E27\u0E32\u0E21\u0E22\u0E32\u0E27\u0E02\
  \u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

การหาความยาวของสตริงหมายถึงการกำหนดว่ามีอักขระในสตริงเท่าไหร่ โปรแกรมเมอร์ทำสิ่งนี้เพื่อตรวจสอบข้อมูลที่ป้อน, ตั้งค่าลูป, หรือเตรียมข้อมูลสำหรับฟังก์ชัน API บางอย่างที่ต้องการขนาดของสตริง

## วิธีการ:

C++ มอบวิธีที่ตรงไปตรงมาในการค้นหาความยาวของสตริงโดยใช้เมธอด `length()` ของคลาส `std::string` แต่ถ้าคุณเป็นสายคลาสสิก คุณยังคงสามารถใช้สตริงแบบ C และ `strlen()` ต่อไปนี้คือตัวอย่างทั้งสองวิธี:

```C++
#include <iostream>
#include <string>
#include <cstring>

int main() {
    // ใช้ std::string
    std::string greeting = "Hello, World!";
    std::cout << "ความยาวของสตริง (std::string): " << greeting.length() << std::endl;

    // ใช้สตริงแบบ C
    const char *c_greeting = "Hello, World!";
    std::cout << "ความยาวของสตริง (สไตล์ C): " << strlen(c_greeting) << std::endl;

    return 0;
}
```

ตัวอย่างผลลัพธ์:
```
ความยาวของสตริง (std::string): 13
ความยาวของสตริง (สไตล์ C): 13
```

## การศึกษาเพิ่มเติม:

เดิมที C++ ได้รับอาร์เรย์อักขระแบบ C และฟังก์ชัน `strlen()` จาก C `strlen()` คำนวณความยาวโดยเดินผ่านอาร์เรย์จนกระทั่งไปถึงอักขระ null, `'\0'`. นี่เป็นกลยุทธ์ที่ง่ายแต่มีประสิทธิภาพ แต่ไม่สามารถเอาชนะประสิทธิภาพของ `std::string.length()` ซึ่งโดยทั่วไปจะเก็บความยาวไว้เพื่อการเรียกใช้ที่รวดเร็ว

มีทางเลือกอื่นไหม? แน่นอน:
- คุณยังสามารถใช้เมธอด `size()`, ที่เหมือนกับ `length()` สำหรับ `std::string`
- สำหรับสตริงอักขระแบบกว้าง, `std::wstring` และเมธอด `length()` ของมันจะเป็นตัวช่วย
- ตัวเลือกที่น่าระทึกขวัญ ได้แก่ ฟังก์ชันที่กำหนดเองหรือการใช้อัลกอริทึ่มเช่น `std::distance` กับตัวนำไปใช้

แต่ควรระวัง, `std::string::length()` คืนค่าเป็นชนิด `size_t`, ซึ่งเป็นจำนวนเต็มที่ไม่มีเครื่องหมาย, ซึ่งอาจทำให้คุณประสบกับพฤติกรรมที่ไม่คาดคิดหากคุณผสมมันกับชนิดที่มีเครื่องหมายในนิพจน์

## อ่านเพิ่มเติม:

- อ้างอิง C++ สำหรับ `std::string::length()`: https://en.cppreference.com/w/cpp/string/basic_string/length
- อ้างอิง C++ สำหรับ `strlen()`: https://en.cppreference.com/w/cpp/string/byte/strlen
- ข้อมูลเพิ่มเติมเกี่ยวกับ `std::string` กับสตริงแบบ C: https://www.learncpp.com/cpp-tutorial/4-4a-c-style-strings/
- สำหรับผู้ที่ต้องการศึกษาลึกลงไปในคลาส `std::string`: https://en.cppreference.com/w/cpp/string/basic_string
