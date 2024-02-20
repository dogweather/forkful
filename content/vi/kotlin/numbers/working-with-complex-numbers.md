---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:47.401884-07:00
description: "S\u1ED1 ph\u1EE9c m\u1EDF r\u1ED9ng h\u1EC7 th\u1ED1ng s\u1ED1 c\u1EE7\
  a ch\xFAng ta \u0111\u1EC3 bao g\u1ED3m c\u0103n b\u1EADc hai c\u1EE7a c\xE1c s\u1ED1\
  \ \xE2m, n\u01A1i \u0111\u01A1n v\u1ECB '\u1EA3o' i b\u1EB1ng c\u0103n b\u1EADc\
  \ hai c\u1EE7a -1. C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng ch\xFAng\u2026"
lastmod: 2024-02-19 22:04:55.763065
model: gpt-4-0125-preview
summary: "S\u1ED1 ph\u1EE9c m\u1EDF r\u1ED9ng h\u1EC7 th\u1ED1ng s\u1ED1 c\u1EE7a\
  \ ch\xFAng ta \u0111\u1EC3 bao g\u1ED3m c\u0103n b\u1EADc hai c\u1EE7a c\xE1c s\u1ED1\
  \ \xE2m, n\u01A1i \u0111\u01A1n v\u1ECB '\u1EA3o' i b\u1EB1ng c\u0103n b\u1EADc\
  \ hai c\u1EE7a -1. C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng ch\xFAng\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
---

{{< edit_this_page >}}

## Gì và Tại sao?
Số phức mở rộng hệ thống số của chúng ta để bao gồm căn bậc hai của các số âm, nơi đơn vị 'ảo' i bằng căn bậc hai của -1. Các lập trình viên sử dụng chúng trong các lĩnh vực như kỹ thuật, vật lý và xử lý tín hiệu, bởi vì chúng rất tốt trong việc mô hình hóa sóng, dao động và bất cứ thứ gì quay.

## Làm thế nào:

Hãy định nghĩa một class số phức cơ bản trong Kotlin:

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
    
    println("a + b = ${a + b}")  // Đầu ra: a + b = (4.0 + 6.0i)
    println("a - b = ${a - b}")  // Đầu ra: a - b = (-2.0 - 2.0i)
    println("a * b = ${a * b}")  // Đầu ra: a * b = (-5.0 + 10.0i)
}
```

## Tìm hiểu thêm

Số phức được đề cập đầu tiên vào thế kỷ 16, giải quyết các phương trình bậc ba không có nghiệm thực. Kỹ thuật và vật lý được lợi ích rất nhiều từ số phức để phân tích mạch AC và hình sóng. Bạn có thể sử dụng thư viện như `koma` hoặc `ejml` của Kotlin cho công việc nặng nhọc.

Các phép toán trên số phức mô phỏng theo số thực, nhưng với sự chú ý đến đơn vị ảo. Phép nhân, ví dụ, tuân theo tính chất phân phối, nhớ rằng `i^2 = -1`. Đơn vị ảo này cho phép chúng ta biểu diễn các số đa chiều, quan trọng trong các phép tính khoa học khác nhau.

## Xem thêm

Thư viện Toán của Kotlin:

- [koma](https://koma.kyonifer.com/): Một thư viện tính toán khoa học cho Kotlin.

Đọc thêm về Số Phức:

- [Wikipedia: Số Phức](https://en.wikipedia.org/wiki/Complex_number)
