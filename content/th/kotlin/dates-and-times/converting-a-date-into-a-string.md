---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:09.810354-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\
  \u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E41\u0E2A\u0E14\u0E07\u0E40\u0E27\u0E25\u0E32\
  \u0E43\u0E14\u0E40\u0E27\u0E25\u0E32\u0E2B\u0E19\u0E36\u0E48\u0E07\u0E43\u0E19\u0E23\
  \u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E17\u0E35\u0E48\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E14\
  \u0E49\u0E42\u0E14\u0E22\u0E21\u0E19\u0E38\u0E29\u0E22\u0E4C\u2026"
lastmod: '2024-03-17T21:57:56.189271-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\
  \u0E48\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\
  \u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E41\u0E2A\u0E14\u0E07\u0E40\u0E27\u0E25\u0E32\
  \u0E43\u0E14\u0E40\u0E27\u0E25\u0E32\u0E2B\u0E19\u0E36\u0E48\u0E07\u0E43\u0E19\u0E23\
  \u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E17\u0E35\u0E48\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E14\
  \u0E49\u0E42\u0E14\u0E22\u0E21\u0E19\u0E38\u0E29\u0E22\u0E4C \u0E42\u0E1B\u0E23\u0E41\
  \u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E01\u0E32\u0E23\u0E19\
  \u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E41\u0E2A\u0E14\u0E07\u0E27\u0E31\u0E19\
  \u0E17\u0E35\u0E48\u0E43\u0E2B\u0E49\u0E01\u0E31\u0E1A\u0E1C\u0E39\u0E49\u0E43\u0E0A\
  \u0E49\u0E2B\u0E23\u0E37\u0E2D\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E0B\u0E35\u0E40\u0E23\
  \u0E35\u0E22\u0E25\u0E44\u0E25\u0E0B\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E40\
  \u0E1E\u0E37\u0E48\u0E2D\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E40\u0E01\u0E47\u0E1A\
  \u0E2B\u0E23\u0E37\u0E2D\u0E01\u0E32\u0E23\u0E16\u0E48\u0E32\u0E22\u0E42\u0E2D\u0E19\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25."
title: "\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\
  \u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 28
---

## อะไร & ทำไม?
การแปลงวันที่เป็นสตริงหมายถึงการแสดงเวลาใดเวลาหนึ่งในรูปแบบที่อ่านได้โดยมนุษย์ โปรแกรมเมอร์ทำการนี้เพื่อแสดงวันที่ให้กับผู้ใช้หรือเพื่อซีเรียลไลซ์ข้อมูลเพื่อการจัดเก็บหรือการถ่ายโอนข้อมูล

## วิธีการ:
ใน Kotlin คุณสามารถแปลง `Date` เป็น `String` โดยใช้คลาส `SimpleDateFormat` มาดูโค้ดกันเลย:

```kotlin
import java.text.SimpleDateFormat
import java.util.Date

fun main() {
    val date = Date() // สร้างอ็อบเจกต์ Date สำหรับเวลาปัจจุบัน
    val format = SimpleDateFormat("yyyy-MM-dd HH:mm:ss") // กำหนดรูปแบบวันที่
    val dateString = format.format(date) // แปลง Date เป็น String
    println(dateString) // แสดงสตริงวันที่
}
```

ผลลัพธ์ตัวอย่างอาจดูเช่นนี้:

```
2023-03-25 14:45:32
```

## ลงลึกลับ
ก่อนที่ `java.time` จะเข้าสู่เวที `SimpleDateFormat` เคยเป็นตัวเลือกหลักสำหรับการแปลงวันที่เป็นสตริงใน Java และด้วยเหตุผลนี้ก็สืบทอดมายัง Kotlin ด้วย Kotlin ทำงานบน Java Virtual Machine และสามารถทำงานร่วมกับไลบรารีของ Java ได้โดยไม่มีปัญหา

อย่างไรก็ตาม ด้วย Java 8 `java.time` เข้ามามีบทบาทพร้อมกับ `DateTimeFormatter` ที่มาพร้อมกับ API ที่ดีขึ้นอย่างมาก นี่เป็นการเปลี่ยนเกมสำคัญ ที่นำเสนอการจัดการวันที่และเวลาที่ปลอดภัย ไม่เปลี่ยนแปลง และสามารถใช้งานร่วมกันได้ การรองรับของ Kotlin สำหรับสิ่งนี้เป็นลักษณะเนียนตา:

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun main() {
    val currentDate = LocalDateTime.now() // รับวันที่และเวลาปัจจุบัน
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    val formattedDate = currentDate.format(formatter)
    println(formattedDate)
}
```

ตัวเลือกอื่น? แน่นอน สำหรับความต้องการที่ไม่ตามมาตรฐานหรือการจัดการกับไลบรารีวันที่หลายตัว เคยมี Joda-Time เป็นมาตรฐานทอง แต่ในวันนี้ `java.time` สามารถดูแลส่วนใหญ่ได้

ตามรายละเอียดการเป็นรายการทำงาน `SimpleDateFormat` ไม่ได้เป็น thread-safe ซึ่งหมายความว่าอาจมีปัญหาเมื่อใช้งานร่วมกันในสภาพแวดล้อมพร้อมกัน `DateTimeFormatter` ไม่มีปัญหานี้ สร้างครั้งแรก ใช้ตลอดไป—หรืออย่างน้อยก็ในระยะเวลาการใช้งานของแอปพลิเคชั่นของคุณโดยไม่ต้องกังวลมาก

## ดูเพิ่มเติม
- `DateTimeFormatter` JavaDoc สำหรับความต้องการรูปแบบของคุณทั้งหมด: [DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- ถ้าคุณรู้สึกคิดถึงหรือต้องการตัวอย่างสำหรับระบบเดิม นี่คือข้อมูลเกี่ยวกับ `SimpleDateFormat`: [SimpleDateFormat](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
