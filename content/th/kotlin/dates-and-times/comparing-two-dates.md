---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:44:38.996300-06:00
description: "\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E35\
  \u0E22\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2A\u0E2D\u0E07\u0E27\u0E31\u0E19\
  \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\
  \u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2B\u0E19\
  \u0E36\u0E48\u0E07\u0E40\u0E01\u0E34\u0E14\u0E02\u0E36\u0E49\u0E19\u0E01\u0E48\u0E2D\
  \u0E19\u0E2B\u0E23\u0E37\u0E2D\u0E2B\u0E25\u0E31\u0E07\u0E08\u0E32\u0E01\u0E2D\u0E35\
  \u0E01\u0E27\u0E31\u0E19\u0E2B\u0E19\u0E36\u0E48\u0E07\u0E2B\u0E23\u0E37\u0E2D\u0E44\
  \u0E21\u0E48 \u0E2B\u0E23\u0E37\u0E2D\u0E16\u0E49\u0E32\u0E40\u0E1B\u0E47\u0E19\u0E0A\
  \u0E48\u0E27\u0E07\u0E40\u0E27\u0E25\u0E32\u0E40\u0E14\u0E35\u0E22\u0E27\u0E01\u0E31\
  \u0E19\u2026"
lastmod: '2024-03-17T21:57:56.190446-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E35\
  \u0E22\u0E1A\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2A\u0E2D\u0E07\u0E27\u0E31\u0E19\
  \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E15\u0E23\u0E27\u0E08\
  \u0E2A\u0E2D\u0E1A\u0E27\u0E48\u0E32\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2B\u0E19\
  \u0E36\u0E48\u0E07\u0E40\u0E01\u0E34\u0E14\u0E02\u0E36\u0E49\u0E19\u0E01\u0E48\u0E2D\
  \u0E19\u0E2B\u0E23\u0E37\u0E2D\u0E2B\u0E25\u0E31\u0E07\u0E08\u0E32\u0E01\u0E2D\u0E35\
  \u0E01\u0E27\u0E31\u0E19\u0E2B\u0E19\u0E36\u0E48\u0E07\u0E2B\u0E23\u0E37\u0E2D\u0E44\
  \u0E21\u0E48 \u0E2B\u0E23\u0E37\u0E2D\u0E16\u0E49\u0E32\u0E40\u0E1B\u0E47\u0E19\u0E0A\
  \u0E48\u0E27\u0E07\u0E40\u0E27\u0E25\u0E32\u0E40\u0E14\u0E35\u0E22\u0E27\u0E01\u0E31\
  \u0E19\u2026"
title: "\u0E40\u0E1B\u0E23\u0E35\u0E22\u0E1A\u0E40\u0E17\u0E35\u0E22\u0E1A\u0E2A\u0E2D\
  \u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48"
---

{{< edit_this_page >}}

## อะไรและทำไม?

การเปรียบเทียบวันที่สองวันหมายถึงการตรวจสอบว่าวันที่หนึ่งเกิดขึ้นก่อนหรือหลังจากอีกวันหนึ่งหรือไม่ หรือถ้าเป็นช่วงเวลาเดียวกัน โปรแกรมเมอร์ทำเช่นนี้สำหรับงานเช่นการเรียงลำดับเหตุการณ์ การกำหนดการ และการตรวจสอบระยะเวลาระหว่างวันที่

## วิธีการ:

```Kotlin
import java.time.LocalDate

fun main() {
    val date1 = LocalDate.of(2023, 4, 10)
    val date2 = LocalDate.of(2023, 5, 15)

    println(date1.isBefore(date2))  // จริง
    println(date1.isAfter(date2))   // เท็จ
    println(date1.isEqual(date2))   // เท็จ

    // เปรียบเทียบโดยใช้ compareTo
    println(date1.compareTo(date2)) // -1 ถ้า date1 อยู่ก่อน date2
}
```

ผลลัพธ์ตัวอย่าง:

```
จริง
เท็จ
เท็จ
-1
```

## ลงลึก

ในอดีต Java มีคลาส `Date` และ `Calendar` แต่ไม่เป็นมิตรกับผู้ใช้มากนัก Kotlin ใช้คลาสที่คล้ายกันโดยภายใน แต่สนับสนุนการใช้งานแพ็กเกจ `java.time` ที่ถูกนำมาใช้ใน Java 8 เพื่อความชัดเจนและประโยชน์ที่ดีขึ้น

มีทางเลือกอื่นๆ เช่น `Instant` สำหรับการประทับเวลา `ZonedDateTime` สำหรับวันที่เฉพาะเขตเวลา หรือการใช้ไลบรารีของบุคคลที่สาม เช่น Joda-Time ควรคำนึงถึงรายละเอียดการนำไปใช้—`Instant` ใช้การประทับเวลา Unix แบบดั้งเดิม ในขณะที่ `LocalDate` ไม่แสดงที่ละเอียดนี้และจัดการกับวันโดยไม่มีเวลาหรือเขตเวลา

การรู้ว่าคลาสใดเหมาะสมกับความต้องการของคุณที่สุดเป็นสิ่งสำคัญ `LocalDate` เพียงพอสำหรับการเปรียบเทียบวันที่ส่วนใหญ่ แต่สำหรับการเปรียบเทียบช่วงเวลาจริง ๆ ควรพิจารณา `ZonedDateTime` หรือ `Instant`

## ดูเพิ่มเติม

- เอกสารการใช้งานการทำงานกับวันที่และเวลาอย่างเป็นทางการของ Kotlin: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/)
- คู่มือวันที่และเวลาของ Java 8: [https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)
- ไลบรารี Joda-Time: [https://www.joda.org/joda-time/](https://www.joda.org/joda-time/)
