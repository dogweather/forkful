---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:25.771853-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\
  \u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E19\u0E33\u0E2D\u0E2D\u0E1A\u0E40\u0E08\u0E47\
  \u0E01\u0E15\u0E4C\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E21\u0E32\u0E40\u0E40\u0E2A\
  \u0E14\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E17\
  \u0E35\u0E48\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E14\u0E49\u0E0B\u0E36\u0E48\u0E07\u0E15\
  \u0E32\u0E21\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E01\
  \u0E33\u0E2B\u0E19\u0E14\u2026"
lastmod: '2024-03-17T21:57:56.093032-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\
  \u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E19\u0E33\u0E2D\u0E2D\u0E1A\u0E40\u0E08\u0E47\
  \u0E01\u0E15\u0E4C\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E21\u0E32\u0E40\u0E40\u0E2A\
  \u0E14\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E17\
  \u0E35\u0E48\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E14\u0E49\u0E0B\u0E36\u0E48\u0E07\u0E15\
  \u0E32\u0E21\u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E01\
  \u0E33\u0E2B\u0E19\u0E14\u2026"
title: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
---

{{< edit_this_page >}}

## คืออะไร & ทำไม?

การแปลงวันที่เป็นสตริงหมายถึงการนำออบเจ็กต์วันที่มาเเสดงเป็นข้อความที่อ่านได้ซึ่งตามรูปแบบเฉพาะกำหนด เหตุผลที่โปรแกรมเมอร์ทำสิ่งนี้เพื่อเเสดงวันที่ให้กับผู้ใช้หรือเพื่อ serialize สำหรับการเก็บข้อมูลและการเชื่อมต่อเครือข่ายในรูปแบบที่เป็นมิตรกับมนุษย์

## วิธีการ:

Java ทำให้การแปลงวันที่เป็นสตริงง่าย อย่างตรงไปตรงมา คลาส `java.time.format.DateTimeFormatter` คือทางเลือกของคุณ นี่คือตัวอย่างโค้ด:

```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateToStringExample {
    public static void main(String[] args) {
        LocalDate date = LocalDate.now(); // วันที่ของวันนี้
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
        String dateString = date.format(formatter);
        System.out.println(dateString); // ผลลัพธ์อาจเป็น: 20/03/2023, เป็นต้น
    }
}
```

## ลงลึก

ในอดีต, Java ใช้ `SimpleDateFormat` จากแพ็คเกจ `java.text`, แต่มันไม่เป็น thread-safe และนำไปสู่ข้อผิดพลาด ด้วย Java 8, แพ็คเกจ `java.time` ได้นำเสนอคลาสวันที่เวลาที่เป็น thread-safe และ immutable `DateTimeFormatter` เป็นส่วนหนึ่งของแพ็คเกจสมัยใหม่นี้

มีทางเลือกอื่นเช่น `FastDateFormat` จาก Apache Commons และ `DateUtils` จากห้องสมุดต่างๆ อย่างไรก็ตาม, ตัวนักพัฒนา Java ส่วนใหญ่ยังคงใช้ไลบรารีมาตรฐานซึ่งเป็นที่มั่นคงและหลากหลาย

เมื่อทำการจัดรูปแบบ, `DateTimeFormatter` ใช้รูปแบบ `yyyy` สำหรับปี, `MM` สำหรับเดือน, และ `dd` สำหรับวัน มันสามารถจัดการกับรูปแบบที่ซับซ้อนได้, แม้กระทั่งรูปแบบที่เฉพาะเจาะจงตามสถานที่, ด้วยวิธีการ `ofPattern` ที่น่าสังเกตยังคือ `DateTimeFormatter` เป็น immutable และ thread-safe, ฉะนั้นคุณสามารถใช้การอินสแตนซ์ของฟอร์แมตเตอร์เดียวกันได้ในหลายๆ เธรดโดยไม่ต้องมีปัญหาในเรื่องการประสาน

## ดูเพิ่มเติม

- เอกสารออฟฟิเชียล Java ของ Oracle สำหรับ `DateTimeFormatter`: https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html
- สำหรับรูปแบบวันที่และเวลาเพิ่มเติม: https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html#patterns
- ภาพรวมวันที่และเวลาของ Java 8: https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html
