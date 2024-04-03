---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:34.118245-07:00
description: "So s\xE1nh hai ng\xE0y ngh\u0129a l\xE0 ki\u1EC3m tra xem ng\xE0y n\xE0\
  o \u0111\u1EBFn tr\u01B0\u1EDBc ho\u1EB7c sau ng\xE0y kia, ho\u1EB7c n\u1EBFu ch\xFA\
  ng l\xE0 c\xF9ng m\u1ED9t th\u1EDDi \u0111i\u1EC3m. L\u1EADp tr\xECnh vi\xEAn th\u1EF1\
  c hi\u1EC7n \u0111i\u1EC1u n\xE0y cho\u2026"
lastmod: '2024-03-13T22:44:36.617962-06:00'
model: gpt-4-0125-preview
summary: "So s\xE1nh hai ng\xE0y ngh\u0129a l\xE0 ki\u1EC3m tra xem ng\xE0y n\xE0\
  o \u0111\u1EBFn tr\u01B0\u1EDBc ho\u1EB7c sau ng\xE0y kia, ho\u1EB7c n\u1EBFu ch\xFA\
  ng l\xE0 c\xF9ng m\u1ED9t th\u1EDDi \u0111i\u1EC3m."
title: "So s\xE1nh hai ng\xE0y"
weight: 27
---

## Cái gì & Tại sao?

So sánh hai ngày nghĩa là kiểm tra xem ngày nào đến trước hoặc sau ngày kia, hoặc nếu chúng là cùng một thời điểm. Lập trình viên thực hiện điều này cho các nhiệm vụ như sắp xếp sự kiện, lập lịch, và kiểm tra khoảng thời gian giữa các ngày.

## Cách thực hiện:

```Kotlin
import java.time.LocalDate

fun main() {
    val date1 = LocalDate.of(2023, 4, 10)
    val date2 = LocalDate.of(2023, 5, 15)

    println(date1.isBefore(date2))  // true
    println(date1.isAfter(date2))   // false
    println(date1.isEqual(date2))   // false

    // So sánh bằng cách sử dụng compareTo
    println(date1.compareTo(date2)) // -1 nếu date1 đến trước date2
}
```

Kết quả mẫu:

```
true
false
false
-1
```

## Đi sâu vào vấn đề

Trước đây, Java cung cấp các lớp `Date` và `Calendar` nhưng chúng không thân thiện với người sử dụng. Kotlin sử dụng các lớp tương tự phía dưới nhưng khuyến khích sử dụng gói `java.time` được giới thiệu trong Java 8 để có sự rõ ràng và tiện ích tốt hơn.

Có những lựa chọn thay thế như `Instant` cho các dấu thời gian, `ZonedDateTime` cho các ngày cụ thể theo múi giờ, hoặc sử dụng một thư viện bên thứ ba như Joda-Time. Hãy giữ ý các chi tiết triển khai — `Instant` sử dụng dấu thời gian Unix truyền thống trong khi `LocalDate` tách biệt điều này và xử lý một ngày hình thành không có thời gian hoặc múi giờ.

Biết lớp nào phù hợp nhất với nhu cầu của bạn là thiết yếu. `LocalDate` phù hợp cho hầu hết các so sánh ngày, nhưng để so sánh chính xác thời điểm trong thời gian, hãy cân nhắc `ZonedDateTime` hoặc `Instant`.

## Xem thêm

- Tài liệu chính thức của Kotlin về ngày và giờ: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/)
- Hướng dẫn về Ngày và Giờ của Java 8: [https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)
- Thư viện Joda-Time: [https://www.joda.org/joda-time/](https://www.joda.org/joda-time/)
