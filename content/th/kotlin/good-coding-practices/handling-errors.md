---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:47:39.504080-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: Kotlin \u0E21\u0E35 `try`,\
  \ `catch`, `finally`, \u0E41\u0E25\u0E30 `throw` \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\
  \u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E02\u0E49\u0E2D\u0E1C\u0E34\
  \u0E14\u0E1E\u0E25\u0E32\u0E14 \u0E19\u0E35\u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\
  \u0E35\u0E17\u0E35\u0E48\u0E04\u0E38\u0E13\u0E43\u0E0A\u0E49\u0E21\u0E31\u0E19."
lastmod: '2024-03-17T21:57:56.184113-06:00'
model: gpt-4-0125-preview
summary: "Kotlin \u0E21\u0E35 `try`, `catch`, `finally`, \u0E41\u0E25\u0E30 `throw`\
  \ \u0E2A\u0E33\u0E2B\u0E23\u0E31\u0E1A\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\
  \u0E32\u0E23\u0E02\u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14 \u0E19\u0E35\
  \u0E48\u0E04\u0E37\u0E2D\u0E27\u0E34\u0E18\u0E35\u0E17\u0E35\u0E48\u0E04\u0E38\u0E13\
  \u0E43\u0E0A\u0E49\u0E21\u0E31\u0E19."
title: "\u0E01\u0E32\u0E23\u0E08\u0E31\u0E14\u0E01\u0E32\u0E23\u0E01\u0E31\u0E1A\u0E02\
  \u0E49\u0E2D\u0E1C\u0E34\u0E14\u0E1E\u0E25\u0E32\u0E14"
weight: 16
---

## วิธีการ:
Kotlin มี `try`, `catch`, `finally`, และ `throw` สำหรับการจัดการข้อผิดพลาด นี่คือวิธีที่คุณใช้มัน:

```Kotlin
fun main() {
    val numerator = 10
    val denominator = 0

    try {
        val result = numerator / denominator
        println("Result: $result")
    } catch (e: ArithmeticException) {
        println("ไม่สามารถหารด้วยศูนย์ได้, เพื่อน.")
    } finally {
        println("สิ่งนี้เกิดขึ้นไม่ว่าจะอย่างไรก็ตาม.")
    }
}
```

ผลลัพธ์:
```
ไม่สามารถหารด้วยศูนย์ได้, เพื่อน.
สิ่งนี้เกิดขึ้นไม่ว่าจะอย่างไรก็ตาม.
```

หากมีบางอย่างผิดพลาดในบล็อก `try`, การปฏิบัติการจะกระโจนไปที่ `catch` มันจะจับข้อผิดพลาดที่เฉพาะเจาะจงที่ถูกโยนออกมา (`ArithmeticException` ในกรณีนี้) บล็อก `finally` จะทำการรันหลังจากนั้น—ไม่เกี่ยวว่าผลลัพธ์จะเป็นอย่างไร

## การเจาะลึก
บล็อก `try-catch` ได้รับการใช้งานมาตั้งแต่ยุคการเขียนโปรแกรมเริ่มต้น—เหมือนเป็นตาข่ายความปลอดภัย Kotlin ยังมี `throw` สำหรับการโยนข้อผิดพลาดเข้าไปในสนามเอง, และมี `finally` สำหรับโค้ดที่ต้องทำงาน—งานทำความสะอาด, โดยมาก

ทางเลือกอื่น ๆ รวมถึงประเภท `Result` และ `try` ของ Kotlin ในฐานะการแสดงออก

```Kotlin
val result: Result<Int> = try {
    Result.success(numerator / denominator)
} catch (e: ArithmeticException) {
    Result.failure(e)
}
```
วิธีนี้คืนค่าเป็นวัตถุ `Result`—คุณจะได้รับการสำเร็จหรือความล้มเหลวโดยไม่มีดราม่าของข้อยกเว้นที่ไม่ได้จัดการ

การดำเนินการใน Kotlin นั้นดีเพราะคุณสามารถใช้ `try` เหมือนกับการแสดงออก, หมายถึงมันกลับค่ามา ตัวเลือกเหล่านี้ทำให้การจัดการข้อผิดพลาดใน Kotlin มีความหลากหลายอย่างมาก มันเป็นเรื่องของการเลือกเครื่องมือที่เหมาะสมสำหรับงาน, เหมือนกับที่คุณจะทำในโรงงาน

## ดูเพิ่มเติม
- เอกสาร Kotlin เกี่ยวกับข้อยกเว้น: [การจัดการข้อยกเว้นของ Kotlin](https://kotlinlang.org/docs/exception-handling.html)
- เอกสารประเภท `Result` ของ Kotlin: [Kotlin Result](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-result/)
- Effective Java, ฉบับที่ 3, โดย Joshua Bloch—มีข้อมูลเชิงลึกเกี่ยวกับข้อยกเว้น, ถึงแม้ว่ามันจะเฉพาะเจาะจงกับ Java
