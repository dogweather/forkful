---
changelog:
- 2024-03-17, gpt-4-0125-preview, translated from English
date: 2024-03-17 21:53:38.978274-06:00
description: "\u0E27\u0E34\u0E18\u0E35\u0E01\u0E32\u0E23: \u0E21\u0E32\u0E01\u0E33\
  \u0E2B\u0E19\u0E14\u0E04\u0E25\u0E32\u0E2A\u0E08\u0E33\u0E19\u0E27\u0E19\u0E40\u0E0A\
  \u0E34\u0E07\u0E0B\u0E49\u0E2D\u0E19\u0E1E\u0E37\u0E49\u0E19\u0E10\u0E32\u0E19\u0E43\
  \u0E19 Kotlin."
lastmod: '2024-03-17T21:57:56.168017-06:00'
model: gpt-4-0125-preview
summary: "\u0E21\u0E32\u0E01\u0E33\u0E2B\u0E19\u0E14\u0E04\u0E25\u0E32\u0E2A\u0E08\
  \u0E33\u0E19\u0E27\u0E19\u0E40\u0E0A\u0E34\u0E07\u0E0B\u0E49\u0E2D\u0E19\u0E1E\u0E37\
  \u0E49\u0E19\u0E10\u0E32\u0E19\u0E43\u0E19 Kotlin."
title: "\u0E01\u0E32\u0E23\u0E17\u0E33\u0E07\u0E32\u0E19\u0E01\u0E31\u0E1A\u0E15\u0E31\
  \u0E27\u0E40\u0E25\u0E02\u0E0B\u0E31\u0E1A\u0E0B\u0E49\u0E2D\u0E19"
weight: 14
---

## วิธีการ:
มากำหนดคลาสจำนวนเชิงซ้อนพื้นฐานใน Kotlin:

```kotlin
data class Complex(val real: Double, val imaginary: Double) {
    operator fun plus(other: Complex) = Complex(real + other.real, imaginary + other.imaginary)
    operator fun minus(other: Complex) = Complex(real - other.real, imaginary - other.imaginary)
    operator fun times(other: Complex) = Complex(
        real * other.real - imaginary * other.imaginary,
        real * other.imaginary + imaginary * other.real
    )
    
    override fun toString(): String = "($real + ${imaginary}i)"
}

fun main() {
    val a = Complex(1.0, 2.0)
    val b = Complex(3.0, 4.0)
    
    println("a + b = ${a + b}")  // ผลลัพธ์: a + b = (4.0 + 6.0i)
    println("a - b = ${a - b}")  // ผลลัพธ์: a - b = (-2.0 - 2.0i)
    println("a * b = ${a * b}")  // ผลลัพธ์: a * b = (-5.0 + 10.0i)
}
```

## ศึกษาเพิ่มเติม
จำนวนเชิงซ้อนถูกกล่าวถึงครั้งแรกในศตวรรษที่ 16 โดยการแก้สมการลูกบาศก์ที่ขาดวิธีแก้แบบจริง วิศวกรรมและฟิสิกส์ได้ประโยชน์อย่างมากจากจำนวนเชิงซ้อนสำหรับการวิเคราะห์วงจร AC และคลื่นการส่งผ่าน คุณอาจเลือกใช้ไลบรารีของ Kotlin เช่น `koma` หรือ `ejml` สำหรับการทำงานที่หนักหน่วง

การดำเนินการบนจำนวนเชิงซ้อนสะท้อนถึงจำนวนจริง แต่พร้อมทั้งคำนึงถึงหน่วยจินตภาพ การคูณตัวอย่างเช่น ปฏิบัติตามคุณสมบัติการแจกแจง โดยจำไว้ว่า `i^2 = -1` หน่วยจินตภาพช่วยให้เราสามารถแทนค่าตัวเลขหลายมิติได้ ซึ่งมีความสำคัญในการคำนวณทางวิทยาศาสตร์หลายประการ

## ดูเพิ่มเติม
ไลบรารีคณิตศาสตร์ของ Kotlin:

- [koma](https://koma.kyonifer.com/): ไลบรารีการคำนวณทางวิทยาศาสตร์สำหรับ Kotlin

การอ่านเพิ่มเติมเกี่ยวกับจำนวนเชิงซ้อน:

- [Wikipedia: จำนวนเชิงซ้อน](https://en.wikipedia.org/wiki/Complex_number)
