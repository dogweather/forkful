---
title:                "Chuyển đổi một ngày thành chuỗi"
aliases:
- /vi/kotlin/converting-a-date-into-a-string.md
date:                  2024-01-28T21:57:55.517660-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi một ngày thành chuỗi"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/converting-a-date-into-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Chuyển đổi một ngày thành một chuỗi có nghĩa là biểu diễn một khoảnh khắc cụ thể trong định dạng dễ đọc cho con người. Lập trình viên thực hiện điều này để hiển thị ngày cho người dùng hoặc để chuẩn hóa chúng cho việc lưu trữ và truyền dữ liệu.

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
