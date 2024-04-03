---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:02.719062-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1\
  ch \u0111\u1EC3 l\xE0m cho c\xE1c chu\u1ED7i n\u1ED1i l\u1EA1i v\u1EDBi nhau trong\
  \ Kotlin - kh\xF4ng c\u1EA7n d\xF9ng keo."
lastmod: '2024-03-13T22:44:36.590738-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch \u0111\u1EC3 l\xE0m cho c\xE1c\
  \ chu\u1ED7i n\u1ED1i l\u1EA1i v\u1EDBi nhau trong Kotlin - kh\xF4ng c\u1EA7n d\xF9\
  ng keo."
title: "N\u1ED1i chu\u1ED7i k\xFD t\u1EF1"
weight: 3
---

## Cách thực hiện:
Dưới đây là cách để làm cho các chuỗi nối lại với nhau trong Kotlin - không cần dùng keo:

```kotlin
fun main() {
    val firstName = "Jet"
    val lastName = "Brains"
    val company = "Kotlin"

    // Sử dụng toán tử cộng
    val fullName = firstName + " " + lastName 
    println(fullName) // Kết quả: Jet Brains

    // Sử dụng mẫu chuỗi
    val employeeIntro = "Xin chào, tôi là $firstName và tôi làm việc tại $company."
    println(employeeIntro) // Kết quả: Xin chào, tôi là Jet và tôi làm việc tại Kotlin.

    // Sử dụng hàm concat()
    val product = "IntelliJ IDEA"
    val description = " rất tuyệt vời!"
    println(product.concat(description)) // Kết quả: IntelliJ IDEA rất tuyệt vời!
}
```

## Sâu hơn nữa
Nối chuỗi đã tồn tại ngay từ khi chúng ta có chuỗi để ghép nối. Các ngôn ngữ lập trình đã không ngừng phát triển cách họ xử lý nhiệm vụ này. Ngày xưa, bạn có thể thấy những bức tường văn bản được ghép lại với nhau bằng một toán tử `+` đơn giản. Tiến nhanh đến Kotlin hiện đại, và bạn có mẫu chuỗi với các ký hiệu `$` kéo các biến trực tiếp vào trong chuỗi, như phép màu.

Có nhiều lựa chọn phong phú. Nếu hiệu suất là chìa khóa và bạn đang xử lý rất nhiều chuỗi, StringBuilder có thể là người bạn tốt nhất, tránh việc tạo ra nhiều đối tượng chuỗi. Sau đó là hàm `joinToString` lấy một danh sách và nghiền nát chúng lại với nhau được tách bởi một dấu phân cách theo lựa chọn của bạn.

Mỗi phương pháp có những tính năng độc đáo—`plus` dễ dàng nhưng có thể chậm khi sử dụng quá mức; mẫu chuỗi thú vị cho khả năng đọc; `concat()` nhớ lại phương pháp của Java và có vẻ một chút trang trọng; `StringBuilder` và `joinToString` hiệu quả hơn đối với các hoạt động dài.

## Xem thêm
Khám phá sâu hơn vào thế giới của chuỗi Kotlin:

- [Tài liệu Kotlin: Kiểu Cơ bản](https://kotlinlang.org/docs/basic-types.html#string-literals)
