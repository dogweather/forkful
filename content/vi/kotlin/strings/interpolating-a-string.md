---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:09.131525-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Kotlin, \u0111\u01B0\u1EE3c \u1EA3nh h\u01B0\
  \u1EDFng b\u1EDFi c\xE1c ng\xF4n ng\u1EEF hi\u1EC7n \u0111\u1EA1i kh\xE1c, \u0111\
  \xE3 gi\u1EDBi thi\u1EC7u n\u1ED9i suy chu\u1ED7i nh\u01B0 m\u1ED9t ph\u01B0\u01A1\
  ng \xE1n thay th\u1EBF s\u1EA1ch s\u1EBD h\u01A1n cho vi\u1EC7c gh\xE9p\u2026"
lastmod: '2024-04-05T21:53:37.981971-06:00'
model: gpt-4-0125-preview
summary: "Kotlin, \u0111\u01B0\u1EE3c \u1EA3nh h\u01B0\u1EDFng b\u1EDFi c\xE1c ng\xF4\
  n ng\u1EEF hi\u1EC7n \u0111\u1EA1i kh\xE1c, \u0111\xE3 gi\u1EDBi thi\u1EC7u n\u1ED9\
  i suy chu\u1ED7i nh\u01B0 m\u1ED9t ph\u01B0\u01A1ng \xE1n thay th\u1EBF s\u1EA1\
  ch s\u1EBD h\u01A1n cho vi\u1EC7c gh\xE9p n\u1ED1i chu\u1ED7i c\u1EE7a Java."
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
