---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:45:28.536921-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E17\u0E33: \u0E19\u0E35\u0E48\u0E04\u0E37\
  \u0E2D\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E2A\
  \u0E15\u0E23\u0E34\u0E07\u0E15\u0E34\u0E14\u0E01\u0E31\u0E19\u0E43\u0E19 Kotlin\
  \ - \u0E44\u0E21\u0E48\u0E15\u0E49\u0E2D\u0E07\u0E43\u0E0A\u0E49\u0E01\u0E32\u0E27\
  ."
lastmod: '2024-03-17T21:57:56.166089-06:00'
model: gpt-4-0125-preview
summary: "\u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\
  \u0E48\u0E17\u0E33\u0E43\u0E2B\u0E49\u0E2A\u0E15\u0E23\u0E34\u0E07\u0E15\u0E34\u0E14\
  \u0E01\u0E31\u0E19\u0E43\u0E19 Kotlin - \u0E44\u0E21\u0E48\u0E15\u0E49\u0E2D\u0E07\
  \u0E43\u0E0A\u0E49\u0E01\u0E32\u0E27."
title: "\u0E01\u0E32\u0E23\u0E15\u0E48\u0E2D\u0E2A\u0E15\u0E23\u0E34\u0E07"
weight: 3
---

## วิธีทำ:
นี่คือวิธีที่ทำให้สตริงติดกันใน Kotlin - ไม่ต้องใช้กาว:

```kotlin
fun main() {
    val firstName = "Jet"
    val lastName = "Brains"
    val company = "Kotlin"

    // การใช้งานตัวดำเนินการบวก
    val fullName = firstName + " " + lastName 
    println(fullName) // ผลลัพธ์: Jet Brains

    // การใช้แม่แบบสตริง
    val employeeIntro = "Hi, I'm $firstName and I work at $company."
    println(employeeIntro) // ผลลัพธ์: สวัสดี, ฉันชื่อ Jet และฉันทำงานที่ Kotlin.

    // ใช้ฟังก์ชั่น concat()
    val product = "IntelliJ IDEA"
    val description = " ยอดเยี่ยม!"
    println(product.concat(description)) // ผลลัพธ์: IntelliJ IDEA ยอดเยี่ยม!
}
```

## ลงลึก
การต่อสตริงมีมาตั้งแต่เรามีสตริงที่จะมาผูกต่อกัน ภาษาการโปรแกรมได้พัฒนาวิธีการจัดการกับงานนี้อย่างต่อเนื่อง ในยุคแรก คุณจะพบกับข้อความยาวๆที่ถูกเพิ่มเข้าด้วยกันด้วยตัวดำเนินการ `+` ข้ามไปยัง Kotlin สมัยใหม่ และคุณจะพบกับแม่แบบที่มีสัญลักษณ์ `$` ที่ดึงตัวแปรเข้าไปในสตริงเสมือนมีเวทมนตร์

ตัวเลือกมีมากมาย หากประสิทธิภาพเป็นสิ่งสำคัญและคุณกำลังจัดการกับสตริงมากมาย StringBuilder สามารถเป็นเพื่อนที่ดีที่สุดของคุณ โดยหลีกเลี่ยงการสร้างวัตถุสตริงหลายอัน จากนั้นก็มีฟังก์ชั่น `joinToString` ซึ่งรับรายการและผสมมันเข้าด้วยกันโดยคั่นด้วยตัวคั่นตามที่คุณเลือก

แต่ละวิธีมีลักษณะเฉพาะของตัวเอง—`plus` ใช้งานง่ายแต่สามารถช้าลงเมื่อใช้มากเกินไป แม่แบบสตริงดีสำหรับการอ่านได้ง่าย `concat()` นิยามตามวิธีของ Java และรู้สึกเป็นทางการเล็กน้อย `StringBuilder` และ `joinToString` มีประสิทธิภาพมากขึ้นสำหรับงานที่ยาวนาน

## ดูเพิ่มเติม
ดำดิ่งลึกลงไปในโลกของสตริง Kotlin:

- [Kotlin Documentation: ประเภทพื้นฐาน](https://kotlinlang.org/docs/basic-types.html#string-literals)
