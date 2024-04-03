---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:48:51.089318-06:00
description: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C\u0E27\u0E31\u0E19\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\
  \u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E40\u0E1B\u0E47\u0E19\u0E27\u0E31\u0E15\
  \u0E16\u0E38 Date\u2026"
lastmod: '2024-03-17T21:57:56.186517-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E34\u0E40\u0E04\u0E23\u0E32\
  \u0E30\u0E2B\u0E4C\u0E27\u0E31\u0E19\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07\
  \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E41\u0E1B\u0E25\u0E07\
  \u0E02\u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E40\u0E1B\u0E47\u0E19\u0E27\u0E31\u0E15\
  \u0E16\u0E38 Date \u0E01\u0E32\u0E23\u0E14\u0E33\u0E40\u0E19\u0E34\u0E19\u0E01\u0E32\
  \u0E23\u0E19\u0E35\u0E49\u0E40\u0E1B\u0E47\u0E19\u0E2A\u0E34\u0E48\u0E07\u0E1E\u0E37\
  \u0E49\u0E19\u0E10\u0E32\u0E19\u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E41\u0E2D\u0E1B\
  \u0E1E\u0E25\u0E34\u0E40\u0E04\u0E0A\u0E31\u0E19\u0E17\u0E35\u0E48\u0E15\u0E49\u0E2D\
  \u0E07\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E27\u0E31\
  \u0E19\u0E17\u0E35\u0E48\u0E17\u0E35\u0E48\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\u0E1B\
  \u0E49\u0E2D\u0E19\u0E2B\u0E23\u0E37\u0E2D\u0E04\u0E31\u0E14\u0E25\u0E2D\u0E01\u0E21\
  \u0E32\u0E08\u0E32\u0E01\u0E0A\u0E38\u0E14\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E20\
  \u0E32\u0E22\u0E19\u0E2D\u0E01 \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E2A\u0E32\u0E21\u0E32\
  \u0E23\u0E16\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E41\u0E25\u0E30\u0E08\u0E31\u0E14\
  \u0E23\u0E39\u0E1B\u0E41\u0E1A\u0E1A\u0E15\u0E32\u0E21\u0E04\u0E27\u0E32\u0E21\u0E15\
  \u0E49\u0E2D\u0E07\u0E01\u0E32\u0E23\u0E44\u0E14\u0E49\u0E2D\u0E22\u0E48\u0E32\u0E07\
  \u0E07\u0E48\u0E32\u0E22\u0E14\u0E32\u0E22."
title: "\u0E01\u0E32\u0E23\u0E41\u0E22\u0E01\u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E2D\
  \u0E2D\u0E01\u0E08\u0E32\u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 30
---

## คืออะไร & ทำไม?
การแยกวิเคราะห์วันจากสตริงหมายถึงการแปลงข้อความเป็นวัตถุ Date การดำเนินการนี้เป็นสิ่งพื้นฐานสำหรับแอปพลิเคชันที่ต้องการทำงานกับวันที่ที่ผู้ใช้ป้อนหรือคัดลอกมาจากชุดข้อมูลภายนอก ทำให้สามารถจัดการและจัดรูปแบบตามความต้องการได้อย่างง่ายดาย

## วิธีทำ:
Kotlin รองรับการแยกวิเคราะห์วันที่ผ่าน `java.time` แพ็คเกจ ซึ่งเปิดตัวใน Java 8 นี่คือวิธีการง่ายๆ โดยใช้ `LocalDateTime` และรูปแบบเฉพาะ:

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun parseDateFromString(dateString: String): LocalDateTime {
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    return LocalDateTime.parse(dateString, formatter)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateFromString(dateString)
    println(date)  // ผลลัพธ์: 2023-04-01T12:00
}
```

เพื่อความยืดหยุ่นมากขึ้น หรือเพื่อจัดการกับวันที่จากแหล่งข้อมูลภายนอกเช่น API คุณอาจใช้ไลบรารีภายนอก เช่น Joda-Time (แม้ว่าจะน้อยกว่าเดิมตอนนี้ด้วย `java.time` ที่แข็งแกร่ง) อย่างไรก็ตาม การยึดมั่นในวิธีการทันสมัยที่ JDK ให้มาคือสิ่งที่นิยมสำหรับแอปพลิเคชั่น Kotlin ส่วนใหญ่

เพื่อแยกวิเคราะห์วันที่ใน Kotlin โดยไม่ใช้ไลบรารีภายนอก คุณยังสามารถใช้ `SimpleDateFormat` คลาสสำหรับเวอร์ชันก่อน Java 8 หรือระดับ API ของ Android ที่ไม่รองรับ `java.time`:

```kotlin
import java.text.SimpleDateFormat

fun parseDateUsingSimpleDateFormat(dateString: String): java.util.Date {
    val formatter = SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    return formatter.parse(dateString)
}

fun main() {
    val dateString = "2023-04-01 12:00:00"
    val date = parseDateUsingSimpleDateFormat(dateString)
    println(date)  // ผลลัพธ์อาจแตกต่างกันไปตามเขตเวลาของคุณ เช่น Sat Apr 01 12:00:00 GMT 2023
}
```

จำไว้เสมอว่าต้องตั้งค่าโซนเวลาถ้าคุณทำงานกับ `SimpleDateFormat` เพื่อหลีกเลี่ยงการเปลี่ยนแปลงเวลาที่ไม่คาดคิดในวันที่ที่แยกวิเคราะห์
