---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:55.517660-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Kotlin, b\u1EA1n c\xF3 th\u1EC3 chuy\u1EC3\
  n \u0111\u1ED5i m\u1ED9t `Date` th\xE0nh `String` s\u1EED d\u1EE5ng l\u1EDBp `SimpleDateFormat`.\
  \ H\xE3y c\xF9ng vi\u1EBFt m\u1ED9t \xEDt m\xE3."
lastmod: '2024-03-13T22:44:36.616723-06:00'
model: gpt-4-0125-preview
summary: "Trong Kotlin, b\u1EA1n c\xF3 th\u1EC3 chuy\u1EC3n \u0111\u1ED5i m\u1ED9\
  t `Date` th\xE0nh `String` s\u1EED d\u1EE5ng l\u1EDBp `SimpleDateFormat`."
title: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i"
weight: 28
---

## Làm thế nào:
Trong Kotlin, bạn có thể chuyển đổi một `Date` thành `String` sử dụng lớp `SimpleDateFormat`. Hãy cùng viết một ít mã:

```kotlin
import java.text.SimpleDateFormat
import java.util.Date

fun main() {
    val date = Date() // Tạo một đối tượng Date cho thời gian hiện tại
    val format = SimpleDateFormat("yyyy-MM-dd HH:mm:ss") // Xác định mẫu định dạng ngày
    val dateString = format.format(date) // Chuyển Date thành String
    println(dateString) // Xuất chuỗi ngày
}
```

Kết quả mẫu có thể trông như thế này:

```
2023-03-25 14:45:32
```

## Sâu hơn một chút
Trước khi `java.time` xuất hiện, `SimpleDateFormat` là chàng ngốc đáng tin cậy để biến đổi ngày-tháng thành chuỗi trong Java và, thông qua di sản, trong Kotlin. Vâng, Kotlin chạy trên Máy Ảo Java và tương tác một cách thoải mái với thư viện Java.

Tuy nhiên, với Java 8, `java.time` xuất hiện, mang theo `DateTimeFormatter` với một API tinh tế hơn nhiều. Đây là một bước ngoặt, cung cấp khả năng thao tác ngày-giờ an toàn, bất biến và an toàn với luồng. Sự hỗ trợ tự nhiên của Kotlin cho điều này là mạch lạc:

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun main() {
    val currentDate = LocalDateTime.now() // Lấy ngày và giờ hiện tại
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
    val formattedDate = currentDate.format(formatter)
    println(formattedDate)
}
```

Có lựa chọn khác? Chắc chắn. Đối với các yêu cầu không tiêu chuẩn hoặc jong jong giữa nhiều thư viện ngày, tùy chọn bên thứ ba như Joda-Time từng là tiêu chuẩn vàng. Những ngày này, `java.time` đáp ứng hầu hết các yêu cầu.

Về chi tiết triển khai, `SimpleDateFormat` không an toàn với luồng, điều này có nghĩa là nó có thể gặp sự cố khi được sử dụng trong môi trường đồng thời. `DateTimeFormatter` không gặp vấn đề này. Tạo một lần, sử dụng mãi mãi - hoặc ít nhất là trong suốt ứng dụng của bạn mà không phải lo lắng nhiều.

## Xem thêm
- `DateTimeFormatter` JavaDoc cho tất cả nhu cầu về mẫu: [DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- Nếu bạn cảm thấy hoài cổ hoặc cần ví dụ cho các hệ thống cũ, đây là thông tin về `SimpleDateFormat`: [SimpleDateFormat](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
