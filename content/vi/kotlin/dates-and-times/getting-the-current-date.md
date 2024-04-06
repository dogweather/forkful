---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:29.089384-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong l\u1ECBch s\u1EED, ng\xE0y th\xE1ng \u0111\
  \xE3 l\xE0 m\u1ED9t m\u1EDB b\xF2ng bong v\u1EA5n \u0111\u1EC1 cho c\xE1c l\u1EAD\
  p tr\xECnh vi\xEAn. M\xFAi gi\u1EDD, n\u0103m nhu\u1EADn, gi\u1EDD ti\u1EBFt ki\u1EC7\
  m \xE1nh s\xE1ng; ch\xFAng ph\u1EE9c t\u1EA1p.\u2026"
lastmod: '2024-04-05T22:50:50.949479-06:00'
model: gpt-4-0125-preview
summary: "Trong l\u1ECBch s\u1EED, ng\xE0y th\xE1ng \u0111\xE3 l\xE0 m\u1ED9t m\u1EDB\
  \ b\xF2ng bong v\u1EA5n \u0111\u1EC1 cho c\xE1c l\u1EADp tr\xECnh vi\xEAn."
title: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i"
weight: 29
---

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
