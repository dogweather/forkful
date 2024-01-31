---
title:                "Nối chuỗi ký tự"
date:                  2024-01-28T21:57:02.719062-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nối chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/concatenating-strings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Nối chuỗi giống như làm một chiếc sandwich, nhưng thay vì bánh mì và nhân, bạn đang xếp chồng các từ lại với nhau để tạo thành một câu hoặc một cụm từ. Các lập trình viên nối chuỗi để tạo ra các văn bản động, như hiển thị tên người dùng cùng với lời chào, hoặc tạo đường dẫn tệp tin một cách linh hoạt.

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
