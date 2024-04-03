---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:49:37.763976-06:00
description: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\
  \u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\u0E01\
  \u0E44\u0E1F\u0E25\u0E4C\u0E40\u0E02\u0E49\u0E32\u0E2A\u0E39\u0E48\u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13, \u0E42\u0E14\u0E22\
  \u0E1B\u0E01\u0E15\u0E34\u0E17\u0E35\u0E25\u0E30\u0E1A\u0E23\u0E23\u0E17\u0E31\u0E14\
  \ \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\
  \u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E1B\
  \u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25\u0E2B\u0E23\u0E37\u0E2D\u0E27\u0E34\u0E40\
  \u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\
  \u0E48\u0E40\u0E01\u0E47\u0E1A\u0E2D\u0E22\u0E39\u0E48\u0E20\u0E32\u0E22\u0E19\u0E2D\
  \u0E01"
lastmod: '2024-03-17T21:57:56.197813-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\
  \u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E08\u0E32\u0E01\
  \u0E44\u0E1F\u0E25\u0E4C\u0E40\u0E02\u0E49\u0E32\u0E2A\u0E39\u0E48\u0E42\u0E1B\u0E23\
  \u0E41\u0E01\u0E23\u0E21\u0E02\u0E2D\u0E07\u0E04\u0E38\u0E13, \u0E42\u0E14\u0E22\
  \u0E1B\u0E01\u0E15\u0E34\u0E17\u0E35\u0E25\u0E30\u0E1A\u0E23\u0E23\u0E17\u0E31\u0E14\
  \ \u0E42\u0E1B\u0E23\u0E41\u0E01\u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\
  \u0E33\u0E40\u0E0A\u0E48\u0E19\u0E19\u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E1B\
  \u0E23\u0E30\u0E21\u0E27\u0E25\u0E1C\u0E25\u0E2B\u0E23\u0E37\u0E2D\u0E27\u0E34\u0E40\
  \u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\
  \u0E48\u0E40\u0E01\u0E47\u0E1A\u0E2D\u0E22\u0E39\u0E48\u0E20\u0E32\u0E22\u0E19\u0E2D\
  \u0E01."
title: "\u0E01\u0E32\u0E23\u0E2D\u0E48\u0E32\u0E19\u0E44\u0E1F\u0E25\u0E4C\u0E02\u0E49\
  \u0E2D\u0E04\u0E27\u0E32\u0E21"
weight: 22
---

## วิธีการ:
ใน Kotlin, คุณสามารถอ่านไฟล์ข้อความได้อย่างง่ายดายโดยใช้ฟังก์ชัน `readLines()` หรือบล็อก `useLines`

```Kotlin
import java.io.File

fun main() {
    // อ่านทุกบรรทัดพร้อมกัน
    val lines = File("example.txt").readLines()
    lines.forEach { line ->
        println(line)
    }

    // มีประสิทธิภาพมากกว่าสำหรับไฟล์ขนาดใหญ่
    File("example.txt").useLines { lines ->
        lines.forEach { line ->
            println(line)
        }
    }
}
```

ผลลัพธ์ตัวอย่าง (สมมติว่า `example.txt` มีสองบรรทัดที่มี "Hello" และ "World"):

```
Hello
World
```

## ค้นลึก
ในอดีต, การอ่านไฟล์ใน Java อาจจะยาวและไม่สะดวก ด้วย Kotlin, ไลบรารีมาตรฐานให้ส่วนขยายที่เป็นประโยชน์เพื่อทำให้การอ่านไฟล์ง่ายขึ้น

มีทางเลือกอื่นๆ สำหรับการอ่านไฟล์ใน Kotlin:
1. `readText()` อ่านเนื้อหาไฟล์ทั้งหมดเข้าสู่ `String`
2. `bufferedReader()` ให้ `BufferedReader` ซึ่งช่วยให้คุณจัดการกับกรณีการใช้งานที่ซับซ้อนมากขึ้น เช่น การอ่านไฟล์ขนาดใหญ่โดยไม่ใช้หน่วยความจำมากเกินไป

ในเรื่องของการดำเนินการ, เมื่อคุณใช้ `useLines`, มันจะดูแลการปิดไฟล์หลังจากการทำงาน, ป้องกันการรั่วไหลของหน่วยความจำได้ เป็นวิธีการที่เป็นประโยชน์ซึ่งได้รับการสนับสนุนใน Kotlin สำหรับการจัดการทรัพยากรอย่างมีประสิทธิภาพ

## ดูเพิ่มเติม
- เอกสารของ Kotlin เกี่ยวกับการอ่านไฟล์: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- เอกสารของ `BufferedReader` สำหรับกรณีที่ซับซ้อนยิ่งขึ้น: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-buffered-reader/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-buffered-reader/)
