---
title:                "Làm việc với số phức"
aliases:
- /vi/kotlin/working-with-complex-numbers/
date:                  2024-01-28T22:12:47.401884-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với số phức"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
