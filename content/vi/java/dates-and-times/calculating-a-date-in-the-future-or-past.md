---
title:                "Tính toán ngày trong tương lai hoặc quá khứ"
aliases:
- /vi/java/calculating-a-date-in-the-future-or-past/
date:                  2024-01-28T21:55:46.268832-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tính toán ngày trong tương lai hoặc quá khứ"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/java/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc tính toán một ngày trong tương lai hoặc quá khứ liên quan đến việc điều chỉnh một ngày đã biết theo một số lượng ngày, tháng hoặc năm nhất định. Các lập trình viên thực hiện việc này cho các tính năng như nhắc nhở, ngày hết hạn, và lên lịch cho các sự kiện.

## Làm thế nào:

```java
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

public class TinhToanNgay {
    public static void main(String[] args) {
        LocalDate homNay = LocalDate.now();
        // Thêm 10 ngày vào ngày hiện tại
        LocalDate ngayTuongLai = homNay.plusDays(10);
        System.out.println("Ngày Tương Lai: " + ngayTuongLai);

        // Trừ đi 2 tháng từ ngày hiện tại
        LocalDate ngayQuaKhu = homNay.minus(2, ChronoUnit.MONTHS);
        System.out.println("Ngày Quá Khứ: " + ngayQuaKhu);
    }
}
```

Kết quả có thể trông giống như này:

```
Ngày Tương Lai: 2023-04-30
Ngày Quá Khứ: 2023-02-20
```

## Tìm hiểu sâu hơn

Trước Java 8, việc thao tác với ngày tháng là một cực hình. Những lớp cũ như `java.util.Date` và `java.util.Calendar` thường gặp lỗi và không thân thiện với người dùng. Gói `java.time` được giới thiệu trong Java 8 đã khắc phục điều này với các lớp được suy nghĩ kĩ càng như `LocalDate`, `LocalTime`, và `ZonedDateTime`.

Có lựa chọn khác không? Trong kỷ nguyên trước Java 8, các thư viện bên thứ ba như Joda-Time khá phổ biến. Ngày nay, bạn vẫn có thể sử dụng chúng, nhưng gói `java.time` chuẩn được khuyến nghị vì nó là một phần chính thức của Java và xử lý mùa giờ, múi giờ, và năm nhuận một cách tinh tế.

Khi lập trình tính toán ngày tháng, hãy xem xét đến múi giờ nếu bối cảnh của bạn cần đến nó. Đối với UTC, sử dụng `Instant` thay vì `LocalDate`. Đối với các múi giờ cụ thể, bạn thường sẽ sử dụng `ZonedDateTime`. Nhớ rằng, các thao tác ngày giờ có thể được nối tiếp, như `date.minusWeeks(1).plusHours(3)`, làm cho mã của bạn gọn gàng hơn.

## Xem thêm

1. Tổng quan về gói `java.time`: [Tài liệu Oracle](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
2. Xử lý múi giờ với `ZonedDateTime`: [Oracle ZonedDateTime](https://docs.oracle.com/javase/8/docs/api/java/time/ZonedDateTime.html)
3. Mẫu ngày và giờ chính thức cho `java.time.format.DateTimeFormatter`: [Oracle DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
