---
title:                "Lấy ngày hiện tại"
date:                  2024-01-28T22:01:29.089384-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lấy ngày hiện tại"

category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
