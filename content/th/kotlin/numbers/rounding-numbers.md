---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:50:14.449023-06:00
description: "\u0E01\u0E32\u0E23\u0E1B\u0E31\u0E14\u0E40\u0E28\u0E29\u0E15\u0E31\u0E27\
  \u0E40\u0E25\u0E02\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E1B\
  \u0E23\u0E31\u0E1A\u0E15\u0E31\u0E27\u0E40\u0E25\u0E02\u0E43\u0E2B\u0E49\u0E40\u0E02\
  \u0E49\u0E32\u0E43\u0E01\u0E25\u0E49\u0E01\u0E31\u0E1A\u0E08\u0E33\u0E19\u0E27\u0E19\
  \u0E40\u0E15\u0E47\u0E21\u0E17\u0E35\u0E48\u0E43\u0E01\u0E25\u0E49\u0E17\u0E35\u0E48\
  \u0E2A\u0E38\u0E14\u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E23\u0E30\
  \u0E14\u0E31\u0E1A\u0E04\u0E27\u0E32\u0E21\u0E41\u0E21\u0E48\u0E19\u0E22\u0E33\u0E17\
  \u0E35\u0E48\u0E01\u0E33\u0E2B\u0E19\u0E14\u2026"
lastmod: '2024-03-17T21:57:56.169043-06:00'
model: gpt-4-0125-preview
summary: "\u0E01\u0E32\u0E23\u0E1B\u0E31\u0E14\u0E40\u0E28\u0E29\u0E15\u0E31\u0E27\
  \u0E40\u0E25\u0E02\u0E2B\u0E21\u0E32\u0E22\u0E16\u0E36\u0E07\u0E01\u0E32\u0E23\u0E1B\
  \u0E23\u0E31\u0E1A\u0E15\u0E31\u0E27\u0E40\u0E25\u0E02\u0E43\u0E2B\u0E49\u0E40\u0E02\
  \u0E49\u0E32\u0E43\u0E01\u0E25\u0E49\u0E01\u0E31\u0E1A\u0E08\u0E33\u0E19\u0E27\u0E19\
  \u0E40\u0E15\u0E47\u0E21\u0E17\u0E35\u0E48\u0E43\u0E01\u0E25\u0E49\u0E17\u0E35\u0E48\
  \u0E2A\u0E38\u0E14\u0E2B\u0E23\u0E37\u0E2D\u0E44\u0E1B\u0E22\u0E31\u0E07\u0E23\u0E30\
  \u0E14\u0E31\u0E1A\u0E04\u0E27\u0E32\u0E21\u0E41\u0E21\u0E48\u0E19\u0E22\u0E33\u0E17\
  \u0E35\u0E48\u0E01\u0E33\u0E2B\u0E19\u0E14\u2026"
title: "\u0E01\u0E32\u0E23\u0E1B\u0E31\u0E14\u0E40\u0E28\u0E29\u0E02\u0E2D\u0E07\u0E15\
  \u0E31\u0E27\u0E40\u0E25\u0E02"
---

{{< edit_this_page >}}

## อะไร & ทำไม?

การปัดเศษตัวเลขหมายถึงการปรับตัวเลขให้เข้าใกล้กับจำนวนเต็มที่ใกล้ที่สุดหรือไปยังระดับความแม่นยำที่กำหนด นักโปรแกรมเมอร์ทำการปัดเศษเพื่อปรับปรุงความสามารถในการอ่าน, ลดความต้องการพื้นที่จัดเก็บ, หรือเพราะค่าที่แน่นอนไม่สำคัญสำหรับการคำนวณต่อไป

## วิธีการ:

ใน Kotlin, สามารถทำการปัดเศษได้โดยใช้ฟังก์ชั่นต่างๆ เช่น `roundToInt()`, `roundToDouble()`, และการใช้ `BigDecimal` เพื่อควบคุมได้มากขึ้น:

```kotlin
fun main() {
    val number1 = 3.14159
    println(number1.roundToInt()) // แสดงผล: 3

    val number2 = 3.5
    println(number2.roundToInt()) // แสดงผล: 4

    val number3 = 123.456
    println("%.2f".format(number3)) // แสดงผล: 123.46
    
    val bigDecimal = number3.toBigDecimal().setScale(1, RoundingMode.HALF_EVEN)
    println(bigDecimal) // แสดงผล: 123.5
}
```

## ลงลึก

ในอดีต, การปัดเศษตัวเลขได้รับการมองว่าเป็นแนวคิดพื้นฐานทั้งในเรื่องของคณิตศาสตร์และการคำนวณ, มีการออกแบบมาเพื่อจัดการกับข้อจำกัดเกี่ยวกับความแม่นยำของตัวเลข ในยุคต้นๆ ของการคำนวณ, การปัดเศษมีความสำคัญเนื่องจากค่าใช้จ่ายของหน่วยความจำที่สูง

ใน Kotlin, การปัดเศษได้รับการสร้างขึ้นบนไลบรารีมาตรฐานของ Java ตัวเลือกสำหรับการปัดเศษ ได้แก่ `Math.round()`, ซึ่งปัดเศษไปยังจำนวนเต็มที่ใกล้ที่สุด, และ `BigDecimal` สำหรับการปัดเศษที่สามารถกำหนดได้เอง, โดยคุณสามารถระบุสเกลและ `RoundingMode`

`RoundingMode` แต่ละอย่างมีนโยบายที่แตกต่างกันสำหรับการจัดการกับการเสมอ (เมื่อตัวเลขอยู่ตรงกลางของตัวเลือกสำหรับการปัดเศษ) ตัวอย่างเช่น, `RoundingMode.HALF_UP` จะปัดเศษไปยังเพื่อนบ้านที่ใกล้ที่สุด, เว้นแต่เพื่อนบ้านทั้งสองอยู่ห่างเท่ากัน, ในกรณีนั้นจะปัดเศษขึ้น

## ดูเพิ่มเติม

- เอกสารการเอกสารของ Kotlin เกี่ยวกับ [`BigDecimal`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/java.math.-big-decimal/index.html)
- เอกสารของ Oracle เกี่ยวกับ Java เกี่ยวกับ [`RoundingMode`](https://docs.oracle.com/javase/8/docs/api/java/math/RoundingMode.html)
- มาตรฐาน IEEE สำหรับ Floating-Point Arithmetic (IEEE 754) [มาตรฐาน IEEE 754](https://ieeexplore.ieee.org/document/4610935)
