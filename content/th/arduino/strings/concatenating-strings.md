---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:09.323241-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: \u0E21\u0E32\u0E40\u0E23\u0E34\
  \u0E48\u0E21\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E01\u0E31\u0E19!\
  \ \u0E17\u0E31\u0E49\u0E07\u0E2B\u0E21\u0E14\u0E17\u0E33\u0E43\u0E19 setup \u0E40\
  \u0E1E\u0E23\u0E32\u0E30\u0E40\u0E23\u0E32\u0E15\u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23\
  \u0E14\u0E39\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\u0E27\
  \u2014\u0E44\u0E21\u0E48\u0E08\u0E33\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E49\u0E2D\u0E07\
  \u0E43\u0E0A\u0E49\u0E25\u0E39\u0E1B\u0E17\u0E35\u0E48\u0E17\u0E33\u0E0B\u0E49\u0E33\
  ."
lastmod: '2024-03-17T21:57:56.473949-06:00'
model: gpt-4-0125-preview
summary: "\u0E21\u0E32\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E48\u0E2D\u0E2A\u0E15\
  \u0E23\u0E34\u0E07\u0E01\u0E31\u0E19."
title: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 3
---

## วิธีทำ:
มาเริ่มต่อสตริงกัน! ทั้งหมดทำใน setup เพราะเราต้องการดูอย่างรวดเร็ว—ไม่จำเป็นต้องใช้ลูปที่ทำซ้ำ

```arduino
void setup() {
  // เริ่มการสื่อสารแบบซีเรียล
  Serial.begin(9600);

  // สร้างสตริงสองตัว
  String greeting = "Hello, ";
  String name = "Arduino!";

  // ต่อสตริง
  String combined = greeting + name;

  // พิมพ์ผลลัพธ์
  Serial.println(combined); 
}
void loop() {
  // ไม่มีอะไรให้ทำซ้ำที่นี่
}
```

คุณรัน, และผลลัพธ์ก็รอคอยคุณใน Serial Monitor:

```
Hello, Arduino!
```

## จัดการข้อมูลลึก
การต่อสตริงเป็นเรื่องที่เก่าแก่ในการเขียนโปรแกรม—มีมาตั้งแต่ภาษาเริ่มต้นพัฒนา ใน Arduino, คุณสามารถใช้ตัวดำเนินการ `+` เหมือนที่เราทำ, หรือใช้ `+=` เพื่อเติมสตริงเข้าไปในสตริงที่มีอยู่ ในเบื้องหลัง, ตัวดำเนินการเหล่านี้จริง ๆ แล้วเรียกฟังก์ชันที่จัดการการจองหน่วยความจำและการคัดลอกตัวอักษรอย่างมีประสิทธิภาพ

ทำไมไม่ต่อสตริงเสมอไป? หากคุณกำลังจัดการกับไมโครคอนโทรลเลอร์ขนาดเล็กและทำการรวมสตริงจำนวนมาก, คุณอาจพบปัญหาเรื่องหน่วยความจำ—เพราะทุกครั้งที่คุณรวม, คุณสร้างสตริงใหม่, ใช้หน่วยความจำเพิ่มขึ้น สำหรับการจัดการสตริงอย่างหนัก, บางครั้งผู้คนอาจใช้แอเรย์ของตัวอักขระ (รูปแบบ C คลาสสิก) เพื่อประหยัดพื้นที่และหลีกเลี่ยงปัญหาเรื่องประสิทธิภาพที่อาจเกิดขึ้น

อย่าลืมตรวจสอบฟังก์ชันสตริงเช่น `concat()`, ซึ่งสามารถเพิ่มไม่เพียงแต่สตริงแต่ยังรวมถึงประเภทข้อมูลอื่นๆ เข้าไปในสตริงที่มีอยู่

## ดูเพิ่มเติม
กำลังมองหาข้อมูลเพิ่มเติม? นี่คือที่ลึกขึ้น:
- อ้างอิงสตริงของ Arduino: [arduino.cc/reference/en/language/variables/data-types/string/](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- การจัดการหน่วยความจำใน Arduino: [learn.adafruit.com/memories-of-an-arduino](https://learn.adafruit.com/memories-of-an-arduino)
- ความชั่วร้ายของสตริง Arduino: [majenko.co.uk/blog/evils-arduino-strings](https://majenko.co.uk/blog/evils-arduino-strings)
