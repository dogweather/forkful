---
aliases:
- /vi/kotlin/finding-the-length-of-a-string/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:38.088282-07:00
description: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i c\xF3 ngh\u0129\
  a l\xE0 \u0111\u1EBFm c\xE1c k\xFD t\u1EF1 c\u1EE7a n\xF3. L\u1EADp tr\xECnh vi\xEA\
  n th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3 x\xE1c nh\u1EADn \u0111\u1EA7\
  u v\xE0o, l\u1EB7p qua c\xE1c k\xFD t\u1EF1, ho\u1EB7c ph\xE2n b\u1ED5 b\u1ED9\u2026"
lastmod: 2024-02-18 23:08:50.640902
model: gpt-4-0125-preview
summary: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i c\xF3 ngh\u0129a l\xE0\
  \ \u0111\u1EBFm c\xE1c k\xFD t\u1EF1 c\u1EE7a n\xF3. L\u1EADp tr\xECnh vi\xEAn th\u1EF1\
  c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3 x\xE1c nh\u1EADn \u0111\u1EA7u v\xE0o,\
  \ l\u1EB7p qua c\xE1c k\xFD t\u1EF1, ho\u1EB7c ph\xE2n b\u1ED5 b\u1ED9\u2026"
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
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
