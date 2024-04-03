---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:24.458866-06:00
description: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\
  \u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\
  \u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E04\u0E49\u0E14\u0E17\u0E35\
  \u0E48\u0E04\u0E32\u0E14\u0E01\u0E32\u0E23\u0E13\u0E4C\u0E41\u0E25\u0E30\u0E08\u0E31\
  \u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E2A\u0E16\u0E32\u0E19\u0E01\u0E32\u0E23\
  \u0E13\u0E4C\u0E17\u0E35\u0E48\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E44\u0E14\
  \u0E49 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\
  \u0E33\u0E2A\u0E34\u0E48\u0E07\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\
  \u0E33\u0E43\u0E2B\u0E49\u0E0B\u0E2D\u0E1F\u0E15\u0E4C\u0E41\u0E27\u0E23\u0E4C\u0E21\
  \u0E35\u0E04\u0E27\u0E32\u0E21\u0E40\u0E2A\u0E16\u0E35\u0E22\u0E23\u2026"
lastmod: '2024-03-17T21:57:56.089009-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\
  \u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\
  \u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E42\u0E04\u0E49\u0E14\u0E17\u0E35\
  \u0E48\u0E04\u0E32\u0E14\u0E01\u0E32\u0E23\u0E13\u0E4C\u0E41\u0E25\u0E30\u0E08\u0E31\
  \u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E2A\u0E16\u0E32\u0E19\u0E01\u0E32\u0E23\
  \u0E13\u0E4C\u0E17\u0E35\u0E48\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14\u0E44\u0E14\
  \u0E49 \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\
  \u0E33\u0E2A\u0E34\u0E48\u0E07\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E17\
  \u0E33\u0E43\u0E2B\u0E49\u0E0B\u0E2D\u0E1F\u0E15\u0E4C\u0E41\u0E27\u0E23\u0E4C\u0E21\
  \u0E35\u0E04\u0E27\u0E32\u0E21\u0E40\u0E2A\u0E16\u0E35\u0E22\u0E23 \u0E1B\u0E49\u0E2D\
  \u0E07\u0E01\u0E31\u0E19\u0E01\u0E32\u0E23\u0E1E\u0E31\u0E07\u0E41\u0E25\u0E30\u0E1E\
  \u0E24\u0E15\u0E34\u0E01\u0E23\u0E23\u0E21\u0E17\u0E35\u0E48\u0E41\u0E1B\u0E25\u0E01\
  \u0E1B\u0E23\u0E30\u0E2B\u0E25\u0E32\u0E14."
title: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E02\
  \u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14"
weight: 16
---

## วิธีการ:
Java ใช้ข้อยกเว้น (exceptions) ในการจัดการกับข้อผิดพลาด คุณสามารถครอบโค้ดที่มีความเสี่ยงด้วยบล็อก `try` และจับข้อยกเว้นด้วย `catch` นี่คือตัวอย่างง่ายๆ:

```java
public class ErrorHandlingExample {
    public static void main(String[] args) {
        try {
            int result = divide(10, 0);
            System.out.println("Result is: " + result);
        } catch (ArithmeticException e) {
            System.out.println("Oops, can't divide by zero!");
        }
    }

    private static int divide(int numerator, int denominator) {
        return numerator / denominator;
    }
}
```

ผลลัพธ์:
```
Oops, can't divide by zero!
```

## การศึกษาลึก
การจัดการข้อผิดพลาดใน Java มีการพัฒนามาอย่างยาวนาน ในช่วงแรกไม่มีการใช้ข้อยกเว้น โปรแกรมเมอร์ตรวจสอบรหัสข้อผิดพลาด จากนั้น Java ได้นำเสนอบล็อก try-catch ทำให้การจัดการข้อผิดพลาดมีความสง่างามมากขึ้น

ทางเลือกที่แตกต่างไปจาก `try-catch` ที่เดิม ได้แก่ `try-with-resources` สำหรับการปิดรีซอร์สโดยอัตโนมัติและโค้ดที่สะอาดขึ้น ซึ่งได้รับการแนะนำใน Java 7

รายละเอียดของการประยุกต์ใช้มีความสำคัญ เช่น การจับ `Exception` หรือ `Throwable` มักจะเป็นปฏิบัติการที่ไม่ดี เพราะมันกว้างเกินไป ทำให้คุณอาจไม่ทันสังเกตุความผิดพลาดที่ไม่ทราบอยู่ ควรยึดติดกับข้อยกเว้นที่เฉพาะเจาะจง

## ดูเพิ่มเติม
- แบบฝึกหัดอย่างเป็นทางการจาก Oracle เกี่ยวกับข้อยกเว้น: [https://docs.oracle.com/javase/tutorial/essential/exceptions/](https://docs.oracle.com/javase/tutorial/essential/exceptions/)
- เอกสารของการแสดงคำสั่ง `try-with-resources` ใน Java: [https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html](https://docs.oracle.com/javase/tutorial/essential/exceptions/tryResourceClose.html)
- Effective Java โดย Joshua Bloch, สำหรับปฏิบัติที่ดีที่สุดเกี่ยวกับข้อยกเว้น
