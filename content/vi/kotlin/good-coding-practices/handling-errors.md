---
title:                "Xử lý lỗi"
aliases:
- /vi/kotlin/handling-errors.md
date:                  2024-01-28T22:02:39.447034-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xử lý lỗi"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/handling-errors.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Xử lý lỗi là cách mã của bạn đối diện với những vấn đề phát sinh trong quá trình thực thi - giống như việc xử lý một tình huống bất ngờ mà không làm rơi nó. Lập trình viên làm việc này để ngăn chặn sự cố và mang lại trải nghiệm mượt mà cho người dùng.

## Làm thế nào:
Kotlin cung cấp `try`, `catch`, `finally`, và `throw` để quản lý lỗi. Dưới đây là cách bạn sử dụng chúng:

```Kotlin
fun main() {
    val numerator = 10
    val denominator = 0

    try {
        val result = numerator / denominator
        println("Kết quả: $result")
    } catch (e: ArithmeticException) {
        println("Không thể chia cho không, bạn nhé.")
    } finally {
        println("Điều này xảy ra bất kể trường hợp nào.")
    }
}
```

Kết quả:
```
Không thể chia cho không, bạn nhé.
Điều này xảy ra bất kể trường hợp nào.
```

Nếu có điều gì đó không ổn trong khối `try`, việc thực thi sẽ chuyển nhanh sang `catch`. Nó bắt lỗi cụ thể được ném ra (`ArithmeticException` trong trường hợp này). Khối `finally` chạy sau - không quan trọng kết quả là gì.

## Tìm hiểu sâu hơn
Khối `try-catch` đã tồn tại từ những ngày đầu lập trình - nó giống như một lưới an toàn. Kotlin cũng cung cấp `throw` để tự tay ném một ngoại lệ vào vòng, và có `finally` dành cho mã phải chạy - thường là công việc dọn dẹp.

Các phương án khác bao gồm kiểu `Result` và việc sử dụng `try` của Kotlin như một biểu thức.

```Kotlin
val result: Result<Int> = try {
    Result.success(numerator / denominator)
} catch (e: ArithmeticException) {
    Result.failure(e)
}
```
Cách tiếp cận này trả về một đối tượng `Result` - bạn nhận được hoặc là thành công hoặc là thất bại mà không gặp phải drama của một ngoại lệ không được xử lý.

Việc triển khai trong Kotlin được coi là gọn gàng bởi bạn có thể sử dụng `try` như một biểu thức, có nghĩa là nó trả về một giá trị. Những lựa chọn như thế này làm cho việc xử lý lỗi trong Kotlin trở nên rất linh hoạt. Nó giống như việc chọn công cụ phù hợp cho công việc, giống như bạn làm trong một xưởng.

## Xem thêm
- Tài liệu Kotlin về Ngoại lệ: [Xử lý Ngoại lệ Kotlin](https://kotlinlang.org/docs/exception-handling.html)
- Tài liệu về kiểu `Result` của Kotlin: [Kotlin Result](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-result/)
- Effective Java, Ấn bản thứ 3, của Joshua Bloch - có cái nhìn sâu sắc về ngoại lệ, mặc dù đặc thù cho Java.
