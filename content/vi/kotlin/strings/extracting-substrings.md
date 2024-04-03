---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:53.616165-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Kotlin, s\u1EED d\u1EE5ng c\xE1c h\xE0\
  m `substring`, `take`, v\xE0 `drop`."
lastmod: '2024-03-13T22:44:36.586610-06:00'
model: gpt-4-0125-preview
summary: "Trong Kotlin, s\u1EED d\u1EE5ng c\xE1c h\xE0m `substring`, `take`, v\xE0\
  \ `drop`."
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
weight: 6
---

## Làm thế nào:
Trong Kotlin, sử dụng các hàm `substring`, `take`, và `drop`.

```Kotlin
fun main() {
    val text = "Hello, Kotlin!"

    println(text.substring(7, 13)) // In ra "Kotlin"
    
    // Từ đầu
    println(text.take(5)) // In ra "Hello"

    // Từ cuối
    println(text.takeLast(6)) // In ra "Kotlin!"

    // Bỏ các ký tự
    println(text.drop(7)) // In ra "Kotlin!"
}
```

## Sâu hơn
Trong những ngày đầu của lập trình, việc xử lý chuỗi là thủ công và dễ mắc lỗi. Trong Kotlin, nó dễ dàng hơn, an toàn hơn và ít tốn tài nguyên hơn, nhờ vào các hàm tích hợp sẵn và các tính năng của lớp String.

Các phương pháp thay thế cho `substring` bao gồm sử dụng biểu thức chính quy với `Regex` hoặc `split` để cắt nhỏ chuỗi - nhưng những phương pháp này có thể quá mức cho những nhiệm vụ đơn giản.

Về mặt triển khai, hãy nhớ rằng chuỗi là bất biến trong Kotlin. Vì vậy, khi bạn trích xuất một chuỗi con, thực chất bạn đang tạo một đối tượng String mới, không phải thay đổi chuỗi gốc.

## Xem thêm
- Tài liệu về chuỗi Kotlin: [Kotlin Strings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- Regex trong Kotlin để thao tác chuỗi nâng cao: [Kotlin Regex](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
