---
aliases:
- /vi/kotlin/calculating-a-date-in-the-future-or-past/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:00.470110-07:00
description: "T\xEDnh to\xE1n m\u1ED9t ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7\
  c qu\xE1 kh\u1EE9 c\xF3 ngh\u0129a l\xE0 t\xECm m\u1ED9t ng\xE0y c\u1EE5 th\u1EC3\
  \ tr\u01B0\u1EDBc ho\u1EB7c sau m\u1ED9t ng\xE0y \u0111\xE3 bi\u1EBFt. C\xE1c l\u1EAD\
  p tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y cho\u2026"
lastmod: 2024-02-18 23:08:50.666165
model: gpt-4-0125-preview
summary: "T\xEDnh to\xE1n m\u1ED9t ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1\
  \ kh\u1EE9 c\xF3 ngh\u0129a l\xE0 t\xECm m\u1ED9t ng\xE0y c\u1EE5 th\u1EC3 tr\u01B0\
  \u1EDBc ho\u1EB7c sau m\u1ED9t ng\xE0y \u0111\xE3 bi\u1EBFt. C\xE1c l\u1EADp tr\xEC\
  nh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y cho\u2026"
title: "T\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1 kh\u1EE9"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Tính toán một ngày trong tương lai hoặc quá khứ có nghĩa là tìm một ngày cụ thể trước hoặc sau một ngày đã biết. Các lập trình viên thực hiện việc này cho các tính năng như nhắc nhở, thông báo hết hạn, hoặc công cụ lên kế hoạch—bất kỳ thứ gì liên quan đến thời gian.

## Cách thực hiện:

Kotlin xử lý ngày và giờ với thư viện `java.time`. Để thêm hoặc bớt ngày, sử dụng `plusDays()` hoặc `minusDays()`. Dưới đây là cách làm:

```kotlin
import java.time.LocalDate

fun main() {
    val hômNay = LocalDate.now()
    val mườiNgàySau = hômNay.plusDays(10)
    val mườiNgàyTrước = hômNay.minusDays(10)
    
    println("Hôm nay: $hômNay")
    println("Mười ngày từ bây giờ: $mườiNgàySau")
    println("Mười ngày trước: $mườiNgàyTrước")
}
```

Kết quả mẫu:

```
Hôm nay: 2023-03-15
Mười ngày từ bây giờ: 2023-03-25
Mười ngày trước: 2023-03-05
```

Ngoài ngày, bạn cũng có thể thao tác với tháng và năm (`plusMonths()`, `minusMonths()`, `plusYears()`, `minusYears()`).

## Sâu hơn

Việc tính toán ngày không phải là mới mẻ. Kể từ Java 8, gói `java.time` đã trở thành lựa chọn hàng đầu cho việc tính toán ngày-giờ—tốt hơn nhiều so với `Calendar` hoặc `Date` cũ, chúng cồng kềnh và không an toàn với luồng.

`java.time` sử dụng các đối tượng bất biến, vì vậy bạn tránh được các lỗi khó chịu từ việc vô tình chỉnh sửa ngày của mình. Các đối tượng như `LocalDate`, `LocalTime`, `LocalDateTime`, và `ZonedDateTime` giúp bạn biểu diễn các khía cạnh khác nhau của thời gian một cách chính xác.

Có các phương án thay thế không? Dĩ nhiên. Trước `java.time`, Joda-Time là lựa chọn hàng đầu. Một số hệ thống cũ vẫn sử dụng nó. Và trong lĩnh vực Android, thư viện ThreeTenABP backport các tính năng của `java.time` để tương thích với Java 6 & 7.

API `java.time` cũng được thiết kế để nhận thức về múi giờ, nhờ các lớp như `ZonedDateTime`. Vì vậy, khi bạn điều chỉnh ngày, bạn có thể tôn trọng theo thời gian quay của Trái Đất.

## Xem thêm

- Tài liệu chính thức `java.time`: [Java SE Ngày Giờ](https://docs.oracle.com/javase/tutorial/datetime/)
- Đối với các nhà phát triển Android, chi tiết thư viện `ThreeTenABP`: [ThreeTenABP trên GitHub](https://github.com/JakeWharton/ThreeTenABP)
- Một hướng dẫn chi tiết, nếu bạn muốn tìm hiểu sâu hơn về ngày và giờ: [Ngày và Giờ trong Java](https://www.baeldung.com/java-8-date-time-intro)
