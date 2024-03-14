---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:53.616165-07:00
description: "Tr\xEDch xu\u1EA5t chu\u1ED7i con c\xF3 ngh\u0129a l\xE0 l\u1EA5y ra\
  \ c\xE1c ph\u1EA7n c\u1EE5 th\u1EC3 t\u1EEB m\u1ED9t chu\u1ED7i. Ch\xFAng ta th\u1EF1\
  c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 thao t\xE1c ho\u1EB7c ph\xE2n t\xED\
  ch d\u1EEF li\u1EC7u v\u0103n b\u1EA3n, nh\u01B0 l\u1EA5y t\xEAn\u2026"
lastmod: '2024-03-13T22:44:36.586610-06:00'
model: gpt-4-0125-preview
summary: "Tr\xEDch xu\u1EA5t chu\u1ED7i con c\xF3 ngh\u0129a l\xE0 l\u1EA5y ra c\xE1\
  c ph\u1EA7n c\u1EE5 th\u1EC3 t\u1EEB m\u1ED9t chu\u1ED7i. Ch\xFAng ta th\u1EF1c\
  \ hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 thao t\xE1c ho\u1EB7c ph\xE2n t\xED\
  ch d\u1EEF li\u1EC7u v\u0103n b\u1EA3n, nh\u01B0 l\u1EA5y t\xEAn\u2026"
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Trích xuất chuỗi con có nghĩa là lấy ra các phần cụ thể từ một chuỗi. Chúng ta thực hiện điều này để thao tác hoặc phân tích dữ liệu văn bản, như lấy tên người dùng từ địa chỉ email hoặc cắt ngày để lấy tháng.

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
