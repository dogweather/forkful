---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:38.088282-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: ."
lastmod: '2024-03-13T22:44:36.589413-06:00'
model: gpt-4-0125-preview
summary: .
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
weight: 7
---

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
