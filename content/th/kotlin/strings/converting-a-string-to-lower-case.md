---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:31.471036-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E1F\u0E31\u0E07\u0E01\
  \u0E4C\u0E0A\u0E31\u0E19 `toLowerCase()` \u0E02\u0E2D\u0E07 Kotlin \u0E17\u0E33\u0E43\
  \u0E2B\u0E49\u0E25\u0E31\u0E01\u0E29\u0E13\u0E30\u0E02\u0E2D\u0E07\u0E2D\u0E31\u0E01\
  \u0E02\u0E23\u0E30\u0E17\u0E31\u0E49\u0E07\u0E2B\u0E21\u0E14\u0E43\u0E19\u0E2A\u0E15\
  \u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E15\u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\
  \u0E4C\u0E40\u0E25\u0E47\u0E01\u0E44\u0E14\u0E49\u0E2D\u0E22\u0E48\u0E32\u0E07\u0E23\
  \u0E27\u0E14\u0E40\u0E23\u0E47\u0E27 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\
  \u0E18\u0E35\u0E17\u0E35\u0E48\u0E04\u0E38\u0E13\u0E43\u0E0A\u0E49\u0E21\u0E31\u0E19\
  ."
lastmod: '2024-03-17T21:57:56.161119-06:00'
model: gpt-4-0125-preview
summary: "\u0E1F\u0E31\u0E07\u0E01\u0E4C\u0E0A\u0E31\u0E19 `toLowerCase()` \u0E02\u0E2D\
  \u0E07 Kotlin \u0E17\u0E33\u0E43\u0E2B\u0E49\u0E25\u0E31\u0E01\u0E29\u0E13\u0E30\
  \u0E02\u0E2D\u0E07\u0E2D\u0E31\u0E01\u0E02\u0E23\u0E30\u0E17\u0E31\u0E49\u0E07\u0E2B\
  \u0E21\u0E14\u0E43\u0E19\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\u0E15\
  \u0E31\u0E27\u0E1E\u0E34\u0E21\u0E1E\u0E4C\u0E40\u0E25\u0E47\u0E01\u0E44\u0E14\u0E49\
  \u0E2D\u0E22\u0E48\u0E32\u0E07\u0E23\u0E27\u0E14\u0E40\u0E23\u0E47\u0E27 \u0E19\u0E35\
  \u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E04\u0E38\u0E13\
  \u0E43\u0E0A\u0E49\u0E21\u0E31\u0E19."
title: "\u0E41\u0E1B\u0E25\u0E07\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E40\u0E1B\u0E47\u0E19\
  \u0E15\u0E31\u0E27\u0E40\u0E25\u0E47\u0E01"
weight: 4
---

## วิธีการ:
ฟังก์ชัน `toLowerCase()` ของ Kotlin ทำให้ลักษณะของอักขระทั้งหมดในสตริงเป็นตัวพิมพ์เล็กได้อย่างรวดเร็ว นี่คือวิธีที่คุณใช้มัน:

```kotlin
fun main() {
    val originalString = "ThiS iS A MixED cAsE String!"
    val lowerCaseString = originalString.lowercase()

    println(lowerCaseString) // ผลลัพธ์: this is a mixed case string!
}
```
เรียกใช้ `lowercase()` แล้วเสร็จ ไม่ว่าอินพุตจะเป็นตัวพิมพ์ใหญ่หรือเล็ก ผลลัพธ์ทั้งหมดจะเป็นตัวพิมพ์เล็ก

## ลึกซึ้ง
Kotlin ไม่ได้สร้างฟีเจอร์การเปลี่ยนเป็นตัวพิมพ์เล็กใหม่โดยสิ้นเชิง จริง ๆ แล้วเป็นคุณสมบัติทั่วไปในภาษาโปรแกรมมิ่ง โดยประวัติศาสตร์ ฟังก์ชันเช่น C ของ `tolower()` ได้จัดการกับการแปลงตัวพิมพ์มาอย่างยาวนาน

ตอนนี้, มีสองประเด็นเมื่อทำการแปลงเป็นตัวพิมพ์เล็ก: ตัวแปรท้องถิ่นและประสิทธิภาพ Kotlin ของ `lowercase()` สามารถรับ `Locale` เพราะ, น่าแปลก, การระบุตัวพิมพ์ไม่เป็นสากล. ตัวอย่างเช่น, ตัว 'I' ที่มีจุดและไม่มีจุดของตุรกีมีพฤติกรรมที่แตกต่างกันในการแปลงตัวพิมพ์

ประสิทธิภาพ? ในแอปส่วนใหญ่, คุณจะไม่สังเกตเห็น แต่การประมวลผลข้อความขนาดใหญ่จะใช้หน่วยความจำและเวลามากขึ้นเพราะสตริงใน Kotlin เป็น immutable  เมื่อคุณทำตัวพิมพ์เล็กสตริง, คุณจะได้รับสตริงใหม่

คนรุ่นเก่าจำได้ `.toLowerCase()` — Kotlin ตอนนี้ต้องการ `lowercase()` เพื่อความชัดเจน

## ดูเพิ่มเติม
- Kotlin String Documentation: [Kotlinlang.org](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/lowercase.html)
- สำหรับการประมวลผลข้อความและการจัดการตัวอักษรขั้นสูง, ตรวจสอบ API ของ `java.lang.String`: [Oracle Docs](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/String.html)
- การทำความเข้าใจเกี่ยวกับตัวแปรท้องถิ่นและความแตกต่างด้านภาษา: [Oracle Locale Docs](https://docs.oracle.com/javase/tutorial/i18n/locale/)
