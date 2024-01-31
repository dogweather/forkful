---
title:                "Phân tích ngày từ chuỗi kí tự"
date:                  2024-01-28T22:04:24.969044-07:00
model:                 gpt-4-0125-preview
simple_title:         "Phân tích ngày từ chuỗi kí tự"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?
Phân tích cú pháp ngày tháng có nghĩa là chuyển đổi ngày tháng từ định dạng văn bản sang một đối tượng ngày mà chương trình có thể hiểu và thao tác được. Điều này rất quan trọng để đọc dữ liệu từ nhiều nguồn như nhập liệu của người dùng hay từ các tệp, cho phép chương trình xử lý và quản lý ngày tháng một cách nhất quán.

## Cách làm:
Với Kotlin, bạn có thể phân tích cú pháp ngày tháng sử dụng lớp `LocalDateTime` từ gói `java.time`. Hãy phân tích một chuỗi thành ngày.

```kotlin
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

fun main() {
    val dateString = "2023-04-01T15:30:00"
    val formatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    val parsedDate = LocalDateTime.parse(dateString, formatter)
    
    println(parsedDate)  // Mẫu Đầu ra: 2023-04-01T15:30
}
```

## Tìm hiểu sâu hơn
Kotlin không có thư viện ngày và giờ riêng của mình. Thay vào đó, nó dựa vào API `java.time` được giới thiệu trong Java 8, thay thế cho các lớp ngày cũ hơn, ít trực quan hơn như `java.util.Date`.

Một ưu điểm lớn của `java.time` là nó mang lại tính bất biến và an toàn với luồng cho các hoạt động ngày giờ. Trước Java 8, bạn thường phải sử dụng các thư viện bên thứ ba như Joda-Time để xử lý ngày tháng một cách mạnh mẽ.

Khi phân tích ngày tháng, bạn phải đối chiếu chuỗi ngày với định dạng chính xác. Nếu không, bạn sẽ gặp lỗi `DateTimeParseException`. Cách tiếp cận của Kotlin được dựa trên tiêu chuẩn ISO 8601, nhưng bạn có thể tạo các định dạng tùy chỉnh với `DateTimeFormatter` cho các kiểu mẫu chuỗi khác nhau.

Các phương án thay thế cho `LocalDateTime` bao gồm `ZonedDateTime` cho hỗ trợ múi giờ hoặc `LocalDate` và `LocalTime` để phân tích ngày và giờ riêng biệt. Sự linh hoạt của Kotlin với API `java.time` đảm bảo bạn có thể tùy chỉnh phân tích ngày tháng theo nhu cầu của chương trình.

## Xem thêm
- Tài liệu chính thức của Java `DateTimeFormatter`: [https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- Tài liệu Kotlin về Tương tác Java: [https://kotlinlang.org/docs/java-interop.html](https://kotlinlang.org/docs/java-interop.html)
- Định dạng Ngày và Giờ ISO 8601: [https://www.iso.org/iso-8601-date-and-time-format.html](https://www.iso.org/iso-8601-date-and-time-format.html)
