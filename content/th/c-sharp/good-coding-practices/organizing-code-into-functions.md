---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:13.545394-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E08\u0E34\u0E19\u0E15\
  \u0E19\u0E32\u0E01\u0E32\u0E23\u0E27\u0E48\u0E32\u0E04\u0E38\u0E13\u0E21\u0E35\u0E42\
  \u0E04\u0E49\u0E14\u0E17\u0E35\u0E48\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E04\u0E33\u0E17\
  \u0E31\u0E01\u0E17\u0E32\u0E22\u0E2B\u0E25\u0E32\u0E22\u0E04\u0E23\u0E31\u0E49\u0E07\
  \ \u0E2B\u0E32\u0E01\u0E44\u0E21\u0E48\u0E21\u0E35\u0E1F\u0E31\u0E07\u0E01\u0E4C\
  \u0E0A\u0E31\u0E19, \u0E08\u0E30\u0E40\u0E1B\u0E47\u0E19\u0E40\u0E23\u0E37\u0E48\
  \u0E2D\u0E07\u0E22\u0E38\u0E48\u0E07\u0E22\u0E32\u0E01 \u0E41\u0E15\u0E48\u0E14\u0E49\
  \u0E27\u0E22\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19, \u0E08\u0E30\u0E40\
  \u0E1B\u0E47\u0E19\u0E23\u0E30\u0E40\u0E1A\u0E35\u0E22\u0E1A."
lastmod: '2024-03-17T21:57:56.232068-06:00'
model: gpt-4-0125-preview
summary: "\u0E08\u0E34\u0E19\u0E15\u0E19\u0E32\u0E01\u0E32\u0E23\u0E27\u0E48\u0E32\
  \u0E04\u0E38\u0E13\u0E21\u0E35\u0E42\u0E04\u0E49\u0E14\u0E17\u0E35\u0E48\u0E1E\u0E34\
  \u0E21\u0E1E\u0E4C\u0E04\u0E33\u0E17\u0E31\u0E01\u0E17\u0E32\u0E22\u0E2B\u0E25\u0E32\
  \u0E22\u0E04\u0E23\u0E31\u0E49\u0E07 \u0E2B\u0E32\u0E01\u0E44\u0E21\u0E48\u0E21\u0E35\
  \u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19, \u0E08\u0E30\u0E40\u0E1B\u0E47\
  \u0E19\u0E40\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E22\u0E38\u0E48\u0E07\u0E22\u0E32\u0E01\
  \ \u0E41\u0E15\u0E48\u0E14\u0E49\u0E27\u0E22\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\
  \u0E31\u0E19, \u0E08\u0E30\u0E40\u0E1B\u0E47\u0E19\u0E23\u0E30\u0E40\u0E1A\u0E35\
  \u0E22\u0E1A."
title: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E23\u0E30\u0E40\u0E1A\u0E35\u0E22\u0E1A\
  \u0E42\u0E04\u0E49\u0E14\u0E40\u0E02\u0E49\u0E32\u0E44\u0E1B\u0E43\u0E19\u0E1F\u0E31\
  \u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19"
weight: 18
---

## วิธีการ:
จินตนาการว่าคุณมีโค้ดที่พิมพ์คำทักทายหลายครั้ง หากไม่มีฟังก์ชัน, จะเป็นเรื่องยุ่งยาก แต่ด้วยฟังก์ชัน, จะเป็นระเบียบ

```C#
// ไม่มีฟังก์ชัน - ซ้ำซาก
Console.WriteLine("Hello, Amy!");
Console.WriteLine("Hello, Bob!");
Console.WriteLine("Hello, Charlie!");

// มีฟังก์ชัน - สะอาดกว่า
void Greet(string name) {
    Console.WriteLine($"Hello, {name}!");
}

Greet("Amy");
Greet("Bob");
Greet("Charlie");
```

ผลลัพธ์เหมือนกัน, แต่เวอร์ชั่นที่สองเรียบร้อยกว่ามาก

## ลงลึก
กลับไปในสมัยภาษา assembly, คุณจะกระโดดไปยังส่วนต่างๆ ของโค้ดด้วย GOTO—ยุ่งยากและยากต่อการติดตาม ฟังก์ชันเป็นการอัพเลเวลที่สำคัญ, เหมือนกับลิ้นชักที่จัดระเบียบในกล่องเครื่องมือ ทางเลือกอื่น? แน่นอน คุณมีเมธอด, ซึ่งเป็นฟังก์ชันในบริบทของคลาส จากนั้นก็มีแลมบ์ดาและฟังก์ชันอินไลน์สำหรับงานที่รวดเร็วและครั้งเดียว

เกี่ยวกับการประยุกต์ใช้—ฟังก์ชันขนาดเล็กที่มีจุดเน้นชัดเจนเป็นทองคำ พวกมันง่ายต่อการทดสอบและดีบัก ฟังก์ชันขนาดใหญ่ที่มีหน้าที่มากมายสามารถกลายเป็นปัญหาใหญ่, ได้รับชื่อเสียงที่น่าสงสัยว่า "โค้ดสปาเก็ตตี้" คงที่กับหนึ่งหน้าที่ต่อหนึ่งฟังก์ชัน; คุณจะขอบคุณตัวเองในภายหลัง

## ดูเพิ่มเติม
สำหรับเพิ่มเติมเกี่ยวกับฟังก์ชันและปฏิบัติที่ดีที่สุด, ตรวจสอบที่:

- Clean Code โดย Robert C. Martin: หลักการเพื่อรักษาฟังก์ชันของคุณให้เรียบร้อย
- Refactoring โดย Martin Fowler: วิธีการปรับปรุงโค้ดที่มีอยู่
- Microsoft C# Guide บน Methods: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/methods
