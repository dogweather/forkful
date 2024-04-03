---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:04.338915-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Kotlin, l\xE0m tr\xF2n c\xF3 th\u1EC3\
  \ \u0111\u01B0\u1EE3c th\u1EF1c hi\u1EC7n b\u1EB1ng c\xE1ch s\u1EED d\u1EE5ng m\u1ED9\
  t s\u1ED1 h\xE0m nh\u01B0 `roundToInt()`, `roundToDouble()`, v\xE0 s\u1EED d\u1EE5\
  ng `BigDecimal` \u0111\u1EC3\u2026"
lastmod: '2024-03-13T22:44:36.594828-06:00'
model: gpt-4-0125-preview
summary: "Trong Kotlin, l\xE0m tr\xF2n c\xF3 th\u1EC3 \u0111\u01B0\u1EE3c th\u1EF1\
  c hi\u1EC7n b\u1EB1ng c\xE1ch s\u1EED d\u1EE5ng m\u1ED9t s\u1ED1 h\xE0m nh\u01B0\
  \ `roundToInt()`, `roundToDouble()`, v\xE0 s\u1EED d\u1EE5ng `BigDecimal` \u0111\
  \u1EC3 c\xF3 th\xEAm ki\u1EC3m so\xE1t."
title: "L\xE0m tr\xF2n s\u1ED1"
weight: 13
---

## Làm thế nào:
Trong Kotlin, làm tròn có thể được thực hiện bằng cách sử dụng một số hàm như `roundToInt()`, `roundToDouble()`, và sử dụng `BigDecimal` để có thêm kiểm soát:

```kotlin
fun main() {
    val number1 = 3.14159
    println(number1.roundToInt()) // Đầu ra: 3

    val number2 = 3.5
    println(number2.roundToInt()) // Đầu ra: 4

    val number3 = 123.456
    println("%.2f".format(number3)) // Đầu ra: 123.46
    
    val bigDecimal = number3.toBigDecimal().setScale(1, RoundingMode.HALF_EVEN)
    println(bigDecimal) // Đầu ra: 123.5
}
```

## Tìm hiểu kỹ hơn
Trong lịch sử, làm tròn số đã là một khái niệm cơ bản trong cả toán học và tính toán, được thiết kế để xử lý hạn chế về độ chính xác số học. Trong thời gian đầu của ngành tính toán, làm tròn rất quan trọng do chi phí của bộ nhớ cao.

Trong Kotlin, làm tròn được xây dựng dựa trên các thư viện Java tiêu chuẩn. Các tùy chọn để làm tròn bao gồm `Math.round()`, làm tròn đến số nguyên gần nhất, và `BigDecimal` cho việc làm tròn có thể tùy chỉnh, nơi bạn có thể chỉ định một tỷ lệ và một `RoundingMode`.

Mỗi `RoundingMode` có các chính sách khác nhau để xử lý hòa (khi chữ số nằm chính giữa các lựa chọn cho việc làm tròn). Ví dụ, `RoundingMode.HALF_UP` làm tròn đến láng giềng gần nhất, trừ khi cả hai láng giềng đều cách đều nhau, trường hợp đó nó sẽ làm tròn lên.

## Xem thêm
- Tài liệu Kotlin về [`BigDecimal`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/java.math.-big-decimal/index.html)
- Tài liệu Java của Oracle về [`RoundingMode`](https://docs.oracle.com/javase/8/docs/api/java/math/RoundingMode.html)
- Tiêu chuẩn IEEE cho Số học Floating-Point (IEEE 754) [Tiêu chuẩn IEEE 754](https://ieeexplore.ieee.org/document/4610935)
