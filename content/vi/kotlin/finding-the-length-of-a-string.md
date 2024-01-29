---
title:                "Tìm chiều dài của một chuỗi ký tự"
date:                  2024-01-28T22:00:38.088282-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm chiều dài của một chuỗi ký tự"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tìm chiều dài của một chuỗi có nghĩa là đếm các ký tự của nó. Lập trình viên thực hiện việc này để xác nhận đầu vào, lặp qua các ký tự, hoặc phân bổ bộ nhớ.

## Cách thực hiện:
```kotlin
fun main() {
    val greeting = "Xin chào, Thế giới!"
    println(greeting.length)  // in ra 17
}
```
Kết quả:
```
17
```

## Sâu hơn nữa
Trong những ngày đầu của ngành công nghệ thông tin, chuỗi được xử lý khác nhau, thường là với các mảng kết thúc bằng giá trị null trong những ngôn ngữ như C. Kotlin, như một ngôn ngữ hiện đại, cung cấp một thuộc tính `length` tích hợp sẵn cho các đối tượng String.

Có phương án thay thế không? Chà, bạn có thể lặp qua một chuỗi và đếm các ký tự - nhưng tại sao phải phát minh lại cái bánh xe? `length` của Kotlin hiệu quả và đơn giản.

Về cơ bản, `length` trả về số lượng đơn vị mã UTF-16 trong chuỗi. Điều này có nghĩa là đối với hầu hết văn bản (như tiếng Anh), số lượng đơn vị mã khớp với số lượng ký tự. Tuy nhiên, đối với các ký tự nằm ngoài Phạm vi Đa ngôn ngữ Cơ bản (BMP), được biểu diễn bởi hai đơn vị mã (một cặp thay thế), thuộc tính `length` có thể không đồng nhất với số lượng điểm mã Unicode.

## Tham khảo thêm
- Tham khảo Thư viện Chuẩn Kotlin cho Chuỗi: [Chuỗi Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- Hiểu về UTF-16 và biểu diễn ký tự: [Unicode trong Java](https://docs.oracle.com/javase/tutorial/i18n/text/unicode.html)
- Tìm hiểu sâu về cách Kotlin xử lý chuỗi và các chức năng liên quan: [Kotlin cho Các Nhà phát triển Java](https://www.coursera.org/learn/kotlin-for-java-developers)
