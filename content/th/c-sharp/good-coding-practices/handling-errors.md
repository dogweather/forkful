---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:47.502674-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E40\u0E23\u0E32\u0E40\
  \u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E14\u0E49\u0E27\u0E22\u0E1A\u0E25\u0E47\
  \u0E2D\u0E01 try-catch \u0E21\u0E31\u0E19\u0E40\u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E01\
  \u0E31\u0E1A\u0E01\u0E32\u0E23\u0E27\u0E32\u0E07\u0E15\u0E32\u0E02\u0E48\u0E32\u0E22\
  \u0E19\u0E34\u0E23\u0E20\u0E31\u0E22\u0E44\u0E27\u0E49\u0E43\u0E15\u0E49\u0E1C\u0E39\
  \u0E49\u0E40\u0E14\u0E34\u0E19\u0E1A\u0E19\u0E40\u0E0A\u0E37\u0E2D\u0E01\u0E25\u0E21\
  \ \u0E16\u0E49\u0E32\u0E1E\u0E27\u0E01\u0E40\u0E02\u0E32\u0E25\u0E37\u0E48\u0E19\
  \ \u0E1E\u0E27\u0E01\u0E40\u0E02\u0E32\u0E08\u0E30\u0E44\u0E21\u0E48\u0E15\u0E01\
  \u0E25\u0E07\u0E44\u0E1B - \u0E1E\u0E27\u0E01\u0E40\u0E02\u0E32\u0E08\u0E30\u0E16\
  \u0E39\u0E01\u0E08\u0E31\u0E1A."
lastmod: '2024-03-17T21:57:56.233978-06:00'
model: gpt-4-0125-preview
summary: "\u0E40\u0E23\u0E32\u0E40\u0E23\u0E34\u0E48\u0E21\u0E15\u0E49\u0E19\u0E14\
  \u0E49\u0E27\u0E22\u0E1A\u0E25\u0E47\u0E2D\u0E01 try-catch \u0E21\u0E31\u0E19\u0E40\
  \u0E2B\u0E21\u0E37\u0E2D\u0E19\u0E01\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E27\u0E32\u0E07\
  \u0E15\u0E32\u0E02\u0E48\u0E32\u0E22\u0E19\u0E34\u0E23\u0E20\u0E31\u0E22\u0E44\u0E27\
  \u0E49\u0E43\u0E15\u0E49\u0E1C\u0E39\u0E49\u0E40\u0E14\u0E34\u0E19\u0E1A\u0E19\u0E40\
  \u0E0A\u0E37\u0E2D\u0E01\u0E25\u0E21 \u0E16\u0E49\u0E32\u0E1E\u0E27\u0E01\u0E40\u0E02\
  \u0E32\u0E25\u0E37\u0E48\u0E19 \u0E1E\u0E27\u0E01\u0E40\u0E02\u0E32\u0E08\u0E30\u0E44\
  \u0E21\u0E48\u0E15\u0E01\u0E25\u0E07\u0E44\u0E1B - \u0E1E\u0E27\u0E01\u0E40\u0E02\
  \u0E32\u0E08\u0E30\u0E16\u0E39\u0E01\u0E08\u0E31\u0E1A."
title: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E02\
  \u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14"
weight: 16
---

## วิธีการ:
เราเริ่มต้นด้วยบล็อก try-catch มันเหมือนกับการวางตาข่ายนิรภัยไว้ใต้ผู้เดินบนเชือกลม ถ้าพวกเขาลื่น พวกเขาจะไม่ตกลงไป - พวกเขาจะถูกจับ

```C#
using System;

class ErrorHandlingExample {
    static void Main() {
        try {
            int[] numbers = {1, 2, 3};
            Console.WriteLine(numbers[5]);  // อุ๊ปส์, ดัชนีเกินขอบเขต!
        } catch (IndexOutOfRangeException e) {
            Console.WriteLine("จับข้อผิดพลาดได้: " + e.Message);
        }
    }
}
```

ผลลัพธ์ตัวอย่างเมื่อเกิดข้อผิดพลาด:
```
จับข้อผิดพลาดได้: Index was outside the bounds of the array.
```

ตอนนี้เราเพิ่มบล็อก finally - มันเป็นสิ่งที่เกิดขึ้นไม่ว่าอย่างไรก็ตาม เหมือนกับการจ่ายภาษี

```C#
try {
    // โค้ดที่อาจมีปัญหาที่นี่
} catch (SomeSpecificException e) {
    // จัดการกับข้อผิดพลาดนั้นๆ ที่นี่
} finally {
    // โค้ดนี้จะทำงานไม่ว่าจะเกิดอะไรขึ้นด้านบน
    Console.WriteLine("สิ่งนี้ทำงานเสมอ.");
}
```

## การพิจารณาอย่างลึกซึ้ง
การจัดการข้อผิดพลาดมีอยู่ในภาษา C# ตั้งแต่เริ่มแรก ตลอดเวลาได้มีการพัฒนา เมื่อก่อนนั้น โปรแกรมเมอร์พึ่งพารหัสการคืนค่าหรือธงโกลบอลเพื่อส่งสัญญาณปัญหา - เป็นวิธีที่ไม่คล่องตัวและมีโอกาสผิดพลาดสูง

C# ใช้ exceptions เป็นวิธีการที่ทันสมัยกว่า ข้อยกเว้นจะถูกโยนออกมาเมื่อเกิดเหตุการณ์ที่ไม่คาดคิด ลักษณะเหมือนกับการโยนธงในการเล่นฟุตบอล Structured exception handling ด้วยบล็อก try, catch และ finally ทำให้การจัดการช่วงเวลาเหล่านี้ชัดเจนและสะอาดกว่าการตรวจสอบข้อผิดพลาดแบบเก่า

มีทางเลือกอื่นหรือไม่? แน่นอน มี `UnhandledExceptionEventHandler` สำหรับ exceptions ที่ลื่นหลุดมือผ่านไป หรือในโค้ดแบบ async, การจัดการข้อผิดพลาดนั้นบางทีอาจหมุนไปมานิดหน่อยกับอ็อบเจ็กต์ `Task` ที่พกพาข้อยกเว้นของตัวเอง

รายละเอียดการดำเนินการ—คล้ายกับข้อความเล็กๆ น้อยๆ—มีความสำคัญ Exceptions อาจมีราคาแพง ลากดึงประสิทธิภาพลงถ้าถูกโยนอย่างไม่มีข้อควรระวัง ดังนั้น เราจึงใช้พวกมันสำหรับกรณีที่ไม่ธรรมดา เว้นแต่การควบคุมโลจิกประจำวัน

## ดูเพิ่มเติม
- [เอกสารอย่างเป็นทางการเกี่ยวกับข้อยกเว้นใน C#](https://docs.microsoft.com/en-us/dotnet/csharp/fundamentals/exceptions/exception-handling)
- [ปฏิบัติการที่ดีที่สุดในการจัดการข้อยกเว้น C#](https://docs.microsoft.com/en-us/dotnet/standard/exceptions/best-practices-for-exceptions)
