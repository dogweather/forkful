---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:09.131525-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: ."
lastmod: '2024-03-13T22:44:36.582636-06:00'
model: gpt-4-0125-preview
summary: .
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
weight: 8
---

## Cách thực hiện:
```kotlin
fun main() {
    val name = "Alex"
    val age = 29
    // Nội suy các biến vào trong chuỗi
    val greeting = "Hello, my name is $name and I am $age years old."
    println(greeting) // Kết quả: Hello, my name is Alex and I am 29 years old.

    // Biểu thức trong chuỗi
    val announcement = "Next year, I'll be ${age + 1}!"
    println(announcement) // Kết quả: Next year, I'll be 30!
}
```

## Sâu hơn
Kotlin, được ảnh hưởng bởi các ngôn ngữ hiện đại khác, đã giới thiệu nội suy chuỗi như một phương án thay thế sạch sẽ hơn cho việc ghép nối chuỗi của Java. Nó cải thiện khả năng đọc và đơn giản hóa mã.

Trước đây, Java yêu cầu sử dụng ghép nối cồng kềnh với `+`, có thể khó đọc và kém hiệu quả, vì nó tạo ra nhiều đối tượng chuỗi. Phương pháp của Kotlin mạnh mẽ hơn, cho phép không chỉ nhúng biến mà còn đánh giá biểu thức trong chuỗi.

Ở dưới cấp độ, Kotlin biên dịch nội suy này thành các hoạt động `StringBuilder` hoặc ghép nối chuỗi, tùy thuộc vào độ phức tạp, gỡ bỏ gánh nặng cho nhà phát triển.

Các phương án thay thế cho nội suy chuỗi bao gồm các công cụ mẫu cho việc thao tác văn bản rộng lớn, nhưng trong mã, nội suy thường là cách nhanh nhất để bao gồm nội dung động.

## Xem Thêm
- [Tài liệu Kotlin về Mẫu Chuỗi](https://kotlinlang.org/docs/basic-syntax.html#string-templates)
- [API `String` của Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [So sánh hiệu suất ghép nối chuỗi của Java và Kotlin](https://proandroiddev.com/the-cost-of-kotlin-language-features-8f7035e9dcb9)
