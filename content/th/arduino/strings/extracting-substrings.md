---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:32.241420-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E2A\u0E48\u0E27\u0E19\u0E22\u0E48\
  \u0E2D\u0E22\u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\
  \u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E2A\u0E48\u0E27\u0E19\u0E17\
  \u0E35\u0E48\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E40\u0E08\u0E32\u0E30\u0E08\u0E07\u0E02\
  \u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2D\u0E2D\u0E01\u0E21\u0E32 - \u0E40\
  \u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E40\u0E2D\u0E32\
  \u0E0A\u0E34\u0E49\u0E19\u0E1E\u0E32\u0E22\u0E21\u0E32\u0E2B\u0E19\u0E36\u0E48\u0E07\
  \u0E0A\u0E34\u0E49\u0E19 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\
  \u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E41\u0E22\u0E01\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25,\u2026"
lastmod: '2024-03-17T21:57:56.471144-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E2A\u0E48\u0E27\u0E19\u0E22\u0E48\
  \u0E2D\u0E22\u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\
  \u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E2A\u0E48\u0E27\u0E19\u0E17\
  \u0E35\u0E48\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E40\u0E08\u0E32\u0E30\u0E08\u0E07\u0E02\
  \u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2D\u0E2D\u0E01\u0E21\u0E32 - \u0E40\
  \u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E40\u0E2D\u0E32\
  \u0E0A\u0E34\u0E49\u0E19\u0E1E\u0E32\u0E22\u0E21\u0E32\u0E2B\u0E19\u0E36\u0E48\u0E07\
  \u0E0A\u0E34\u0E49\u0E19 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\
  \u0E23\u0E4C\u0E17\u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\
  \u0E48\u0E2D\u0E41\u0E22\u0E01\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25,\u2026"
title: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E22\
  \u0E48\u0E2D\u0E22\u0E2D\u0E2D\u0E01\u0E21\u0E32"
---

{{< edit_this_page >}}

## อะไรและทำไม?

การแยกส่วนย่อยของสตริงหมายถึงการดึงส่วนที่เฉพาะเจาะจงของสตริงออกมา - เหมือนกับการเอาชิ้นพายมาหนึ่งชิ้น โปรแกรมเมอร์ทำเช่นนี้เพื่อแยกข้อมูล, ทำความสะอาดข้อมูลป้อนเข้า, หรือจัดการข้อความสำหรับเรื่องเช่นการวิเคราะห์ข้อความจากเซนเซอร์

## วิธีการ:

สตริงใน Arduino สามารถถูกตัดเป็นส่วนย่อยๆ ได้โดยใช้ `substring()`:

```arduino
void setup() {
  Serial.begin(9600);
  String phrase = "Hello, Arduino World!";
  String greeting = phrase.substring(0, 5);
  String location = phrase.substring(7, 19);
  
  Serial.println(greeting); // พิมพ์ "Hello"
  Serial.println(location); // พิมพ์ "Arduino World"
}

void loop() {
  // ที่นี่ไม่มีอะไรที่ต้องวนซ้ำ.
}
```

ผลลัพธ์บน Serial Monitor:
```
Hello
Arduino World
```

## ลงลึก

ก่อนที่ Arduino จะทำให้มันง่าย, โปรแกรมเมอร์ใช้ arrays ของ char และฟังก์ชันเช่น `strncpy` ในภาษา C มันไม่ได้เป็นเพียงแค่วัตถุโบราณ, มันยังคงถูกใช้งานสำหรับการดำเนินการระดับต่ำ ฟังก์ชัน `substring()` ใน Arduino จริงๆแล้วเป็น wrapper ที่ทำให้มันง่ายขึ้นสำหรับเราเมื่อจัดการกับอ็อบเจกต์ String แต่ต้องระวัง, การใช้ `String` อาจนำไปสู่การแตกตัวของหน่วยความจำ หากความมั่นคงเป็นสิ่งสำคัญ, โดยเฉพาะในโปรแกรมที่ทำงานยาวนานหรือซับซ้อน, ควรพิจารณาวิธีการแบบเก่าของ arrays `char`.

ทางเลือกต่อ `substring()` รวมถึงการจัดการ arrays ของ char โดยตรงหรือฟังก์ชันเช่น `strtok()` ซึ่งอาจมีประสิทธิภาพมากขึ้นแต่อาจทำให้คุณต้องจัดการกับโค้ดเพิ่มเติม

ใต้ฮู้ด, `substring()` สร้างอ็อบเจกต์ String ใหม่ซึ่งมีอักขระตั้งแต่ดัชนีเริ่มต้นไปจนถึงก่อนดัชนีสิ้นสุด, ซึ่งสามารถข้ามได้หากคุณต้องการทุกอย่างจนจบ

## ดูเพิ่มเติม:

- อ้างอิงสตริง Arduino: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- การจัดการหน่วยความจำใน Arduino: https://learn.arduino.cc/programming/variables-and-data-types/memory-management
- วิธี substr ของ `std::string` ใน C++ เพื่อการเปรียบเทียบ: http://www.cplusplus.com/reference/string/string/substr/
