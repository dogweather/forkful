---
title:                "Nội suy chuỗi ký tự"
aliases:
- /vi/kotlin/interpolating-a-string/
date:                  2024-01-28T22:02:09.131525-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nội suy chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Nội suy chuỗi cho phép bạn nhúng trực tiếp các biến vào trong chuỗi. Điều này rất tiện lợi cho việc tạo ra văn bản động, dễ đọc mà không cần đến sự ghép nối cồng kềnh.

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
