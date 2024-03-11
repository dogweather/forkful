---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:29.089384-07:00
description: "Ch\xFAng ta l\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i \u0111\u1EC3 bi\u1EBF\
  t d\u1EEF li\u1EC7u c\u1EE7a ng\xE0y h\xF4m nay. \u0110i\u1EC1u n\xE0y r\u1EA5t\
  \ quan tr\u1ECDng cho h\xE0ng t\u1EA5n t\xEDnh n\u0103ng \u2013 ngh\u0129 \u0111\
  \u1EBFn nh\u1EADt k\xFD, d\xF9ng th\u1EED, s\u1EF1 ki\u1EC7n. B\u1EA1n n\xF3i\u2026"
lastmod: '2024-03-11T00:14:09.887261-06:00'
model: gpt-4-0125-preview
summary: "Ch\xFAng ta l\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i \u0111\u1EC3 bi\u1EBFt d\u1EEF\
  \ li\u1EC7u c\u1EE7a ng\xE0y h\xF4m nay. \u0110i\u1EC1u n\xE0y r\u1EA5t quan tr\u1ECD\
  ng cho h\xE0ng t\u1EA5n t\xEDnh n\u0103ng \u2013 ngh\u0129 \u0111\u1EBFn nh\u1EAD\
  t k\xFD, d\xF9ng th\u1EED, s\u1EF1 ki\u1EC7n. B\u1EA1n n\xF3i\u2026"
title: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Chúng ta lấy ngày hiện tại để biết dữ liệu của ngày hôm nay. Điều này rất quan trọng cho hàng tấn tính năng – nghĩ đến nhật ký, dùng thử, sự kiện. Bạn nói đến nó, ngày thường luôn có mặt ở đó.

## Làm thế nào:
```Kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println("Ngày hôm nay là: $today")
}
```

Mẫu đầu ra:
```
Ngày hôm nay là: 2023-04-05
```

## Sâu hơn nữa
Trong lịch sử, ngày tháng đã là một mớ bòng bong vấn đề cho các lập trình viên. Múi giờ, năm nhuận, giờ tiết kiệm ánh sáng; chúng phức tạp. Kotlin dựa vào các API `java.time` từ Java 8 trở đi, làm cho các hoạt động về ngày dễ chịu hơn.

`LocalDate.now()` là lựa chọn hàng đầu của chúng tôi cho ngày hiện tại. Không có thời gian, không có múi giờ – chỉ có ngày. Cần thời gian? Có `LocalTime`. Cả hai? `LocalDateTime`. Và nếu múi giờ quan trọng, sử dụng `ZonedDateTime`.

Có lựa chọn khác không? Trước Java 8, `java.util.Date` và `Calendar` là chủ đạo. Không tệ, không tuyệt, nhưng bây giờ hơi lỗi thời và frankly, kém trực quan hơn.

Bên trong, `LocalDate.now()` bắt đầu từ đồng hồ hệ thống. Nhưng không phải đồng hồ nào – đó là đồng hồ UTC, được điều chỉnh theo múi giờ mặc định của hệ thống bạn. Bạn có thể túy ý thay đổi nó, chắc chắn – truyền một `Clock` hoặc `ZoneId` khác nếu bạn thích sống mạo hiểm.

## Xem thêm
Tài liệu Kotlin về ngày và giờ: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/)

Tổng quan về Ngày/Giờ Java 8: [https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)

Muốn trở thành nhà sử học toàn diện? Kiểm tra sự tiến hóa của java.time: [https://www.oracle.com/technetwork/articles/java/jf14-date-time-2125367.html](https://www.oracle.com/technetwork/articles/java/jf14-date-time-2125367.html)
