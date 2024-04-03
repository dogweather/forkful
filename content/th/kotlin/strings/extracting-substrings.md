---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:31.454425-06:00
description: "\u0E01\u0E32\u0E23\u0E2A\u0E01\u0E31\u0E14\u0E2A\u0E48\u0E27\u0E19\u0E22\
  \u0E48\u0E2D\u0E22\u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07 (Extracting substrings)\
  \ \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\
  \u0E2A\u0E48\u0E27\u0E19\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E2D\u0E2D\u0E01\u0E08\u0E32\
  \u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07 \u0E17\u0E33\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\
  \u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E2B\u0E23\u0E37\u0E2D\u0E27\u0E34\
  \u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21 \u0E40\u0E0A\u0E48\u0E19\u2026"
lastmod: '2024-03-17T21:57:56.163103-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2A\u0E01\u0E31\u0E14\u0E2A\u0E48\u0E27\u0E19\u0E22\
  \u0E48\u0E2D\u0E22\u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07 (Extracting substrings)\
  \ \u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\
  \u0E2A\u0E48\u0E27\u0E19\u0E40\u0E09\u0E1E\u0E32\u0E30\u0E2D\u0E2D\u0E01\u0E08\u0E32\
  \u0E01\u0E2A\u0E15\u0E23\u0E34\u0E07 \u0E17\u0E33\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E01\
  \u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E2B\u0E23\u0E37\u0E2D\u0E27\u0E34\
  \u0E40\u0E04\u0E23\u0E32\u0E30\u0E2B\u0E4C\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E02\
  \u0E49\u0E2D\u0E04\u0E27\u0E32\u0E21 \u0E40\u0E0A\u0E48\u0E19 \u0E01\u0E32\u0E23\
  \u0E2B\u0E22\u0E34\u0E1A\u0E0A\u0E37\u0E48\u0E2D\u0E1C\u0E39\u0E49\u0E43\u0E0A\u0E49\
  \u0E07\u0E32\u0E19\u0E2D\u0E2D\u0E01\u0E08\u0E32\u0E01\u0E17\u0E35\u0E48\u0E2D\u0E22\
  \u0E39\u0E48\u0E2D\u0E35\u0E40\u0E21\u0E25\u0E2B\u0E23\u0E37\u0E2D\u0E15\u0E31\u0E14\
  \u0E27\u0E31\u0E19\u0E17\u0E35\u0E48\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E23\u0E31\u0E1A\
  \u0E40\u0E14\u0E37\u0E2D\u0E19."
title: "\u0E01\u0E32\u0E23\u0E14\u0E36\u0E07\u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E22\
  \u0E48\u0E2D\u0E22\u0E2D\u0E2D\u0E01\u0E21\u0E32"
weight: 6
---

## อะไร & ทำไม?
การสกัดส่วนย่อยของสตริง (Extracting substrings) หมายถึงการดึงส่วนเฉพาะออกจากสตริง ทำเพื่อการจัดการหรือวิเคราะห์ข้อมูลข้อความ เช่น การหยิบชื่อผู้ใช้งานออกจากที่อยู่อีเมลหรือตัดวันที่เพื่อรับเดือน

## วิธีการ:
ใน Kotlin, ใช้ฟังก์ชัน `substring`, `take`, และ `drop` 

```Kotlin
fun main() {
    val text = "Hello, Kotlin!"

    println(text.substring(7, 13)) // แสดง "Kotlin"
    
    // จากต้น
    println(text.take(5)) // แสดง "Hello"

    // จากท้าย
    println(text.takeLast(6)) // แสดง "Kotlin!"

    // การละทิ้งอักขระ
    println(text.drop(7)) // แสดง "Kotlin!"
}
```

## ลงลึก
ในยุคแรกของการเขียนโปรแกรม การจัดการสตริงเป็นเรื่องที่ต้องทำด้วยมือและส่งผลให้เกิดความผิดพลาดได้ง่าย ใน Kotlin, สิ่งนี้ง่ายขึ้น ปลอดภัยขึ้น และใช้ทรัพยากรน้อยลง ด้วยฟังก์ชันที่มีอยู่และคุณสมบัติของคลาส String

ทางเลือกอื่นๆ ของ `substring` รวมถึงการใช้ regular expressions ด้วย `Regex` หรือ `split` เพื่อตัดสตริง แต่วิธีการเหล่านี้อาจมากเกินไปสำหรับงานง่ายๆ

ในด้านการนำไปใช้งาน, จำไว้ว่าสตริงใน Kotlin เป็น immutable ดังนั้น เมื่อคุณสกัดส่วนย่อยของสตริงออกมา คุณจริงๆ แล้วกำลังสร้าง Object สตริงใหม่ ไม่ใช่เปลี่ยนแปลงสตริงเดิม

## ดูเพิ่มเติม
- เอกสารสตริง Kotlin: [Kotlin Strings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- การใช้ Regex ใน Kotlin สำหรับการจัดการสตริงขั้นสูง: [Kotlin Regex](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
