---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:39.693123-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E01\u0E48\u0E2D\u0E19\
  \ Java 8, \u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\
  \u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E40\u0E1B\u0E47\u0E19\u0E40\u0E23\u0E37\u0E48\
  \u0E2D\u0E07\u0E22\u0E32\u0E01 \u0E04\u0E25\u0E32\u0E2A\u0E40\u0E01\u0E48\u0E32\u0E46\
  \ \u0E40\u0E0A\u0E48\u0E19 `java.util.Date` \u0E41\u0E25\u0E30 `java.util.Calendar`\
  \ \u0E40\u0E15\u0E47\u0E21\u0E44\u0E1B\u0E14\u0E49\u0E27\u0E22\u0E1A\u0E31\u0E4A\
  \u0E01\u0E41\u0E25\u0E30\u0E44\u0E21\u0E48\u0E40\u0E1B\u0E47\u0E19\u0E21\u0E34\u0E15\
  \u0E23\u0E15\u0E48\u0E2D\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u2026"
lastmod: '2024-04-05T22:51:14.020673-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E48\u0E2D\u0E19 Java 8, \u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\
  \u0E32\u0E23\u0E01\u0E31\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E40\u0E1B\u0E47\
  \u0E19\u0E40\u0E23\u0E37\u0E48\u0E2D\u0E07\u0E22\u0E32\u0E01 \u0E04\u0E25\u0E32\u0E2A\
  \u0E40\u0E01\u0E48\u0E32\u0E46 \u0E40\u0E0A\u0E48\u0E19 `java.util.Date` \u0E41\u0E25\
  \u0E30 `java.util.Calendar` \u0E40\u0E15\u0E47\u0E21\u0E44\u0E1B\u0E14\u0E49\u0E27\
  \u0E22\u0E1A\u0E31\u0E4A\u0E01\u0E41\u0E25\u0E30\u0E44\u0E21\u0E48\u0E40\u0E1B\u0E47\
  \u0E19\u0E21\u0E34\u0E15\u0E23\u0E15\u0E48\u0E2D\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\
  \ \u0E41\u0E1E\u0E47\u0E01\u0E40\u0E01\u0E08 `java.time` \u0E17\u0E35\u0E48\u0E19\
  \u0E33\u0E40\u0E2A\u0E19\u0E2D\u0E43\u0E19 Java 8 \u0E44\u0E14\u0E49\u0E41\u0E01\
  \u0E49\u0E44\u0E02\u0E1B\u0E31\u0E0D\u0E2B\u0E32\u0E19\u0E35\u0E49\u0E14\u0E49\u0E27\
  \u0E22\u0E04\u0E25\u0E32\u0E2A\u0E17\u0E35\u0E48\u0E44\u0E14\u0E49\u0E23\u0E31\u0E1A\
  \u0E01\u0E32\u0E23\u0E04\u0E34\u0E14\u0E04\u0E49\u0E19\u0E2D\u0E22\u0E48\u0E32\u0E07\
  \u0E14\u0E35 \u0E40\u0E0A\u0E48\u0E19 `LocalDate`, `LocalTime`, \u0E41\u0E25\u0E30\
  \ `ZonedDateTime` \u0E21\u0E35\u0E17\u0E32\u0E07\u0E40\u0E25\u0E37\u0E2D\u0E01\u0E2D\
  \u0E37\u0E48\u0E19\u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E21\u0E48?"
title: "\u0E01\u0E32\u0E23\u0E04\u0E33\u0E19\u0E27\u0E13\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E43\u0E19\u0E2D\u0E19\u0E32\u0E04\u0E15\u0E2B\u0E23\u0E37\u0E2D\u0E2D\u0E14\
  \u0E35\u0E15"
weight: 26
---

## วิธีการ:
```java
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

public class DateCalculation {
    public static void main(String[] args) {
        LocalDate today = LocalDate.now();
        // เพิ่มวันที่ 10 วันในวันปัจจุบัน
        LocalDate futureDate = today.plusDays(10);
        System.out.println("วันที่ในอนาคต: " + futureDate);

        // ลบออก 2 เดือนจากวันปัจจุบัน
        LocalDate pastDate = today.minus(2, ChronoUnit.MONTHS);
        System.out.println("วันที่ในอดีต: " + pastDate);
    }
}
```

ผลลัพธ์อาจมีลักษณะดังนี้:

```
วันที่ในอนาคต: 2023-04-30
วันที่ในอดีต: 2023-02-20
```

## ตรวจสอบอย่างละเอียด
ก่อน Java 8, การจัดการกับวันที่เป็นเรื่องยาก คลาสเก่าๆ เช่น `java.util.Date` และ `java.util.Calendar` เต็มไปด้วยบั๊กและไม่เป็นมิตรต่อผู้ใช้ แพ็กเกจ `java.time` ที่นำเสนอใน Java 8 ได้แก้ไขปัญหานี้ด้วยคลาสที่ได้รับการคิดค้นอย่างดี เช่น `LocalDate`, `LocalTime`, และ `ZonedDateTime`

มีทางเลือกอื่นหรือไม่? ในยุคก่อน Java 8, ไลบรารีภายนอกเช่น Joda-Time เป็นที่นิยม ในปัจจุบัน, คุณยังสามารถใช้งานได้, แต่แนะนำให้ใช้ `java.time` มาตรฐานเนื่องจากเป็นส่วนหนึ่งของ Java อย่างเป็นทางการและจัดการเวลาออมแสง, เขตเวลา, และปีใหญ่ได้อย่างสง่างาม

เมื่อเขียนคำนวณวันที่, พิจารณาเขตเวลาหากบริบทของคุณต้องการ เพื่อ UTC, ใช้ `Instant` แทน `LocalDate` สำหรับเขตเวลาเฉพาะ, คุณมักจะใช้ `ZonedDateTime` จำไว้ว่า, การดำเนินการวัน-เวลาสามารถเชื่อมโยงกันได้, เช่น `date.minusWeeks(1).plusHours(3)`, ทำให้โค้ดของคุณสะอาดขึ้น

## ดูเพิ่มเติม
1. ภาพรวมแพ็กเกจ `java.time`: [เอกสาร Oracle](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
2. การจัดการเขตเวลาด้วย `ZonedDateTime`: [Oracle ZonedDateTime](https://docs.oracle.com/javase/8/docs/api/java/time/ZonedDateTime.html)
3. รูปแบบวันที่และเวลาอย่างเป็นทางการสำหรับ `java.time.format.DateTimeFormatter`: [Oracle DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
