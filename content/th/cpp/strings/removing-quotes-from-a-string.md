---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:10.430785-06:00
description: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E2D\u0E31\u0E0D\u0E1B\u0E23\u0E30\u0E01\
  \u0E32\u0E28\u0E2D\u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\
  \u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E2B\u0E25\u0E35\u0E01\u0E40\
  \u0E25\u0E35\u0E48\u0E22\u0E07\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E04\u0E39\u0E48\
  \u0E2B\u0E23\u0E37\u0E2D\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E40\u0E14\u0E35\u0E48\
  \u0E22\u0E27\u0E17\u0E35\u0E48\u0E2B\u0E48\u0E2D\u0E2B\u0E38\u0E49\u0E21\u0E02\u0E49\
  \u0E2D\u0E04\u0E27\u0E32\u0E21\u0E02\u0E2D\u0E07\u0E40\u0E23\u0E32 (' \u0E2B\u0E23\
  \u0E37\u0E2D \")\u2026"
lastmod: '2024-03-17T21:57:56.510146-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E2D\u0E31\u0E0D\u0E1B\u0E23\u0E30\u0E01\
  \u0E32\u0E28\u0E2D\u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\
  \u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E2B\u0E25\u0E35\u0E01\u0E40\
  \u0E25\u0E35\u0E48\u0E22\u0E07\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E04\u0E39\u0E48\
  \u0E2B\u0E23\u0E37\u0E2D\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E40\u0E14\u0E35\u0E48\
  \u0E22\u0E27\u0E17\u0E35\u0E48\u0E2B\u0E48\u0E2D\u0E2B\u0E38\u0E49\u0E21\u0E02\u0E49\
  \u0E2D\u0E04\u0E27\u0E32\u0E21\u0E02\u0E2D\u0E07\u0E40\u0E23\u0E32 (' \u0E2B\u0E23\
  \u0E37\u0E2D \")\u2026"
title: "\u0E01\u0E32\u0E23\u0E25\u0E1A\u0E40\u0E04\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E2B\
  \u0E21\u0E32\u0E22\u0E2D\u0E31\u0E0D\u0E1B\u0E23\u0E30\u0E01\u0E32\u0E28\u0E2D\u0E2D\
  \u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07"
---

{{< edit_this_page >}}

## อะไร & ทำไม?
การลบอัญประกาศออกจากสตริงหมายถึงการหลีกเลี่ยงอักขระคู่หรืออักขระเดี่ยวที่ห่อหุ้มข้อความของเรา (' หรือ ") นักพัฒนาโปรแกรมมักทำเช่นนี้เพื่อทำความสะอาดข้อมูลนำเข้า, เก็บข้อความไว้ในฐานข้อมูล, หรือเตรียมสตริงสำหรับการประมวลผลต่อไปโดยไม่มีความรกของเครื่องหมายอ้างอิง

## วิธีทำ:
นี่คือวิธีที่ตรงไปตรงมาในการกำจัดเครื่องหมายอ้างอิงด้วย C++:

```cpp
#include <iostream>
#include <algorithm>

std::string remove_quotes(std::string input) {
    input.erase(std::remove(input.begin(), input.end(), '\"'), input.end());
    input.erase(std::remove(input.begin(), input.end(), '\''), input.end());
    return input;
}

int main() {
    std::string original = R"("Hello, 'World'!")";
    std::string no_quotes = remove_quotes(original);
    std::cout << no_quotes << std::endl;
    return 0;
}
```

เมื่อรันโค้ดนี้, คุณจะได้:

```
Hello, World!
```

วอลลา! เครื่องหมายอ้างอิงหายไป

## ล้ำลึก
อัญประกาศมีความน่ารำคาญในข้อความมาตั้งแต่ยุคแรกๆของการคอมพิวเตอร์ ในสมัยนั้น, คุณจะเห็นนักพัฒนาทุ่มเทการวนลูปผ่านแต่ละอักขระเพื่อกรองออกอัญประกาศนั้นๆ ปัจจุบัน, เรามี `std::remove` ใน Standard Template Library (STL) เพื่อทำงานหนักแทนเรา

มีทางเลือกอื่นหรือ? แน่นอน! คุณอาจใช้ regular expressions กับ `std::regex` เพื่อเป้าหมายไปที่อัญประกาศ, แต่นั่นก็เหมือนกับการใช้ค้อนขนาดใหญ่เพื่อทุบถั่ว - มีพลัง, แต่อาจเกินความจำเป็นสำหรับงานง่ายๆ สำหรับผู้ที่ชื่นชอบ flavor ของ C++ ใหม่ๆ, คุณอาจทดลองใช้ `std::string_view` สำหรับวิธีที่ไม่ต้องการการเปลี่ยนแปลง

ในทางการเขียนโค้ด, จำไว้ว่า `std::remove` ไม่ได้จริงๆ ลบองค์ประกอบออกจากตัวบรรจุ; มันจะจัดเรียงที่องค์ประกอบที่ไม่ถูกลบไปข้างหน้าและคืนค่า iterator ไปที่สิ้นสุดใหม่ของช่วง นั่นคือเหตุผลที่เราต้องการวิธี `erase` เพื่อตัดส่วนท้ายที่ไม่ต้องการ

## ดูเพิ่มเติม
- การอ้างอิง C++ `std::remove`: [cppreference.com](https://en.cppreference.com/w/cpp/algorithm/remove)
- ข้อมูลเพิ่มเติมเกี่ยวกับการจัดการ `std::string`: [cplusplus.com](http://www.cplusplus.com/reference/string/string/)
