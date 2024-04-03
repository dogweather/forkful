---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:55.822749-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Kotlin \u0E21\u0E35\u0E27\
  \u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\
  \u0E4C\u0E17\u0E35\u0E48\u0E15\u0E23\u0E07\u0E44\u0E1B\u0E15\u0E23\u0E07\u0E21\u0E32\
  \ \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\
  \u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\u0E19\u0E42\u0E14\u0E22\u0E44\u0E21\u0E48\u0E15\
  \u0E49\u0E2D\u0E07\u0E21\u0E35\u0E44\u0E25\u0E1A\u0E23\u0E32\u0E23\u0E35\u0E02\u0E2D\
  \u0E07\u0E1A\u0E38\u0E04\u0E04\u0E25\u0E17\u0E35\u0E48\u0E2A\u0E32\u0E21\u0E40\u0E1E\
  \u0E34\u0E48\u0E21\u0E40\u0E15\u0E34\u0E21 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E15\
  \u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E07\u0E48\u0E32\u0E22\u0E46."
lastmod: '2024-03-17T21:57:56.199055-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u0E21\u0E35\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23\u0E40\u0E02\
  \u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E17\u0E35\u0E48\u0E15\u0E23\u0E07\u0E44\
  \u0E1B\u0E15\u0E23\u0E07\u0E21\u0E32 \u0E42\u0E14\u0E22\u0E43\u0E0A\u0E49\u0E44\u0E25\
  \u0E1A\u0E23\u0E32\u0E23\u0E35\u0E21\u0E32\u0E15\u0E23\u0E10\u0E32\u0E19\u0E42\u0E14\
  \u0E22\u0E44\u0E21\u0E48\u0E15\u0E49\u0E2D\u0E07\u0E21\u0E35\u0E44\u0E25\u0E1A\u0E23\
  \u0E32\u0E23\u0E35\u0E02\u0E2D\u0E07\u0E1A\u0E38\u0E04\u0E04\u0E25\u0E17\u0E35\u0E48\
  \u0E2A\u0E32\u0E21\u0E40\u0E1E\u0E34\u0E48\u0E21\u0E40\u0E15\u0E34\u0E21 \u0E19\u0E35\
  \u0E48\u0E04\u0E37\u0E2D\u0E15\u0E31\u0E27\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E07\u0E48\
  \u0E32\u0E22\u0E46."
title: "\u0E01\u0E32\u0E23\u0E40\u0E02\u0E35\u0E22\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 24
---

## วิธีการ:
Kotlin มีวิธีการเขียนไฟล์ที่ตรงไปตรงมา โดยใช้ไลบรารีมาตรฐานโดยไม่ต้องมีไลบรารีของบุคคลที่สามเพิ่มเติม นี่คือตัวอย่างง่ายๆ:

```kotlin
import java.io.File

fun main() {
    val textToWrite = "Hello, Kotlin file writing!"
    File("example.txt").writeText(textToWrite)
}
```
ตัวอย่างโค้ดนี้สร้างไฟล์ที่ชื่อ "example.txt" ใน root directory ของโปรเจค และเขียนสตริง `Hello, Kotlin file writing!` ลงไปในนั้น หากไฟล์นั้นมีอยู่แล้ว มันจะถูกเขียนทับ 

สำหรับการเพิ่มข้อความลงในไฟล์อย่างควบคุมมากขึ้น หรือการเขียนข้อมูลจำนวนมาก คุณสามารถใช้ `appendText` หรือ `bufferedWriter()`:

```kotlin
import java.io.File

fun appendToFile() {
    val moreText = "Appending more text."
    File("example.txt").appendText(moreText)
}

fun writeWithBufferedWriter() {
    val largeText = "Large amounts of text...\nOn multiple lines."
    File("output.txt").bufferedWriter().use { out ->
        out.write(largeText)
    }
}

fun main() {
    appendToFile() // เพิ่มข้อความลงในไฟล์ที่มีอยู่
    writeWithBufferedWriter() // เขียนข้อมูลข้อความจำนวนมากอย่างมีประสิทธิภาพ
}
```

ในฟังก์ชัน `appendToFile`, เรากำลังเพิ่มข้อความเข้าไปใน "example.txt" โดยไม่เขียนทับเนื้อหาปัจจุบันของมัน ฟังก์ชัน `writeWithBufferedWriter` แสดงถึงวิธีที่มีประสิทธิภาพในการเขียนของข้อมูลหรือข้อความจำนวนมาก ซึ่งเป็นวิธีที่เหมาะสำหรับการลดการดำเนินการ I/O เมื่อจัดการกับหลายบรรทัดหรือไฟล์ขนาดใหญ่

ตัวอย่างเหล่านี้ครอบคลุมการดำเนินการพื้นฐานสำหรับการเขียนไฟล์ข้อความใน Kotlin ซึ่งแสดงถึงความง่ายและพลังของไลบรารีมาตรฐานของ Kotlin สำหรับการดำเนินการไฟล์ I/O
