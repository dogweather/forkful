---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:53.851320-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: ."
lastmod: '2024-03-17T21:57:56.518546-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u0E2A\u0E48\u0E07\u0E04\u0E33\u0E02\u0E2D HTTP"
weight: 44
---

## วิธีการ:
```c++
#include <iostream>
#include <cpr/cpr.h> // ตรวจสอบให้แน่ใจว่าติดตั้งไลบรารี CPR ก่อน

int main() {
    cpr::Response r = cpr::Get(cpr::Url{"http://httpbin.org/get"});
    std::cout << r.text << std::endl; // แสดงผลข้อความตอบกลับ
    return 0;
}
```

ตัวอย่างผลลัพธ์:
```json
{
  "args": {},
  "headers": {
    "Accept": "*/*",
    "Host": "httpbin.org",
    "User-Agent": "curl/7.64.1"
  },
  "origin": "0.0.0.0",
  "url": "https://httpbin.org/get"
}
```

## ลงลึก
คำขอ HTTP เป็นสิ่งที่สำคัญตั้งแต่การเริ่มต้นของเว็บ; พวกมันปฏิบัติตามโมเดล client-server ก่อนหน้าไลบรารี C++ เช่น CPR, การส่งคำขอ HTTP โดยทั่วไปหมายถึงการใช้งาน `libcurl` โดยตรง, หรือการทำงานร่วมกับภาษาอื่นที่มีอุปกรณ์ครบครันสำหรับการสื่อสารเว็บ

CPR, ซึ่งย่อมาจาก C++ Requests, เป็น wrapper ที่ใช้งานง่ายแรงบันดาลใจมาจากโมดูล `requests` ของ Python มันซ่อนความซับซ้อนของ `libcurl` ออกไป ยังมีทางเลือกอื่นๆ อยู่ เช่น Boost.Beast สำหรับการทำงาน HTTP/S ที่ระดับต่ำกว่า, หรือไลบรารี POCO ที่เสนอความยืดหยุ่นในการพกพา

การดำเนินการลึกลงไป, การส่งคำขอ HTTP คือการตั้งค่าการเชื่อมต่อ TCP, การจัดรูปแบบคำขอที่สอดคล้องกับโปรโตคอล HTTP, จากนั้นจึงแยกวิเคราะห์การตอบกลับ การทำสิ่งนี้ให้ถูกต้องจากขั้นพื้นฐานไม่ใช่เรื่องง่าย เนื่องจากการจัดการข้อผิดพลาด, ความซับซ้อนของเวอร์ชัน HTTP, และการพิจารณาความปลอดภัย

## ดูเพิ่มเติมได้ที่
- คลังของ CPR บน Github: https://github.com/libcpr/cpr
- เอกสารของ `libcurl` สำหรับการใช้งานขั้นสูง: https://curl.se/libcurl/
- เอกสารอย่างเป็นทางการของ Boost.Beast: https://www.boost.org/doc/libs/release/libs/beast/
- เอกสารไลบรารี POCO C++: https://pocoproject.org/docs/
