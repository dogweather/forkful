---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:46:54.657215-06:00
description: "\u0E01\u0E32\u0E23\u0E2B\u0E32\u0E04\u0E27\u0E32\u0E21\u0E22\u0E32\u0E27\
  \u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E19\u0E31\u0E1A\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E20\
  \u0E32\u0E22\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07 \u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E2A\u0E34\u0E48\u0E07\u0E19\
  \u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E1B\u0E49\u0E2D\u0E19,\
  \ \u0E27\u0E19\u0E25\u0E39\u0E1B\u0E1C\u0E48\u0E32\u0E19\u0E2D\u0E31\u0E01\u0E02\
  \u0E23\u0E30, \u0E2B\u0E23\u0E37\u0E2D\u0E08\u0E31\u0E14\u0E2A\u0E23\u0E23\u0E1E\
  \u0E37\u0E49\u0E19\u0E17\u0E35\u0E48\u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25"
lastmod: '2024-03-17T21:57:56.165102-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E2B\u0E32\u0E04\u0E27\u0E32\u0E21\u0E22\u0E32\u0E27\
  \u0E02\u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\
  \u0E07\u0E01\u0E32\u0E23\u0E19\u0E31\u0E1A\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E20\
  \u0E32\u0E22\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07 \u0E42\u0E1B\u0E23\u0E41\u0E01\
  \u0E23\u0E21\u0E40\u0E21\u0E2D\u0E23\u0E4C\u0E17\u0E33\u0E2A\u0E34\u0E48\u0E07\u0E19\
  \u0E35\u0E49\u0E40\u0E1E\u0E37\u0E48\u0E2D\u0E15\u0E23\u0E27\u0E08\u0E2A\u0E2D\u0E1A\
  \u0E02\u0E49\u0E2D\u0E21\u0E39\u0E25\u0E17\u0E35\u0E48\u0E1B\u0E49\u0E2D\u0E19,\
  \ \u0E27\u0E19\u0E25\u0E39\u0E1B\u0E1C\u0E48\u0E32\u0E19\u0E2D\u0E31\u0E01\u0E02\
  \u0E23\u0E30, \u0E2B\u0E23\u0E37\u0E2D\u0E08\u0E31\u0E14\u0E2A\u0E23\u0E23\u0E1E\
  \u0E37\u0E49\u0E19\u0E17\u0E35\u0E48\u0E40\u0E01\u0E47\u0E1A\u0E02\u0E49\u0E2D\u0E21\
  \u0E39\u0E25"
title: "\u0E04\u0E49\u0E19\u0E2B\u0E32\u0E04\u0E27\u0E32\u0E21\u0E22\u0E32\u0E27\u0E02\
  \u0E2D\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07"
---

{{< edit_this_page >}}

## คืออะไร & ทำไม?
การหาความยาวของสตริงหมายถึงการนับอักขระภายในสตริง โปรแกรมเมอร์ทำสิ่งนี้เพื่อตรวจสอบข้อมูลที่ป้อน, วนลูปผ่านอักขระ, หรือจัดสรรพื้นที่เก็บข้อมูล

## วิธีการ:
```kotlin
fun main() {
    val greeting = "Hello, World!"
    println(greeting.length)  // พิมพ์ 13
}
```
ผลลัพธ์:
```
13
```

## ทำความเข้าใจลึกซึ้ง
ในยุคแรกๆ ของการคอมพิวเตอร์, สตริงถูกจัดการอย่างแตกต่างกัน, บ่อยครั้งด้วยอาร์เรย์ที่สิ้นสุดด้วย null ในภาษาเช่น C Kotlin, ในฐานะภาษาที่ทันสมัย, ให้คุณสมบัติ `length` ในตัวสำหรับวัตถุ String

ทางเลือก? อืม, คุณอาจจะวนลูปผ่านสตริงและนับอักขระ - แต่ทำไมต้องคิดใหม่ทั้งที่มีวิธีที่ทำงานได้ดีและง่ายกว่าอยู่แล้ว? `length` ของ Kotlin มีประสิทธิภาพและเรียบง่าย

ภายใต้ฝาครอบ, `length` จะส่งคืนจำนวนหน่วยโค้ด UTF-16 ในสตริง นั่นหมายความว่าสำหรับข้อความส่วนใหญ่ (เช่น ภาษาอังกฤษ), จำนวนหน่วยโค้ดจะตรงกับจำนวนอักขระ อย่างไรก็ตาม, สำหรับอักขระที่อยู่นอก Basic Multilingual Plane (BMP), ซึ่งแทนด้วยสองหน่วยโค้ด (คู่ surrogate), คุณสมบัติ `length` อาจไม่ตรงกับจำนวนจุดโค้ด Unicode

## ดูเพิ่มเติมได้ที่
- อ้างอิงไลบรารีมาตรฐานของ Kotlin สำหรับ Strings: [Kotlin Strings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- เข้าใจ UTF-16 และการแทนค่าของอักขระ: [Unicode ใน Java](https://docs.oracle.com/javase/tutorial/i18n/text/unicode.html)
- การทำความเข้าใจลึกซึ้งว่า Kotlin จัดการกับสตริงและฟังก์ชั่นที่เกี่ยวข้องอย่างไร: [Kotlin สำหรับผู้พัฒนา Java](https://www.coursera.org/learn/kotlin-for-java-developers)
