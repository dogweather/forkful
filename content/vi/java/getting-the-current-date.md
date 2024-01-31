---
title:                "Lấy ngày hiện tại"
date:                  2024-01-28T22:01:58.834735-07:00
model:                 gpt-4-0125-preview
simple_title:         "Lấy ngày hiện tại"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/java/getting-the-current-date.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại Sao?

Lấy ngày hiện tại trong Java dễ như ăn bánh - chỉ là việc sử dụng đúng lớp để lấy ngày hiện tại từ đồng hồ hệ thống. Các lập trình viên làm điều này để theo dõi, ghi nhật ký, hoặc để làm cho ứng dụng của họ có tính năng nhạy cảm với thời gian.

## Cách thực hiện:

### Lấy Ngày Hôm Nay

```java
import java.time.LocalDate;

public class Main {
    public static void main(String[] args) {
        LocalDate today = LocalDate.now();
        System.out.println("Ngày Hôm Nay: " + today);
    }
}
```

**Kết Quả Mẫu:**
```
Ngày Hôm Nay: 2023-04-01
```

### Thời Gian với Nhiều Chi Tiết Hơn

```java
import java.time.LocalDateTime;

public class Main {
    public static void main(String[] args) {
        LocalDateTime now = LocalDateTime.now();
        System.out.println("Ngày và Giờ Hiện Tại: " + now);
    }
}
```

**Kết Quả Mẫu:**
```
Ngày và Giờ Hiện Tại: 2023-04-01T12:45:30.123
```

## Tìm Hiểu Sâu:

Trước Java 8, `java.util.Date` và `java.util.Calendar` là những lựa chọn thông dụng cho ngày-giờ. Nhưng chúng cồng kềnh và không trực quan. Java 8 giới thiệu `java.time`, một API mạnh mẽ và dễ hiểu hơn. `java.time.LocalDate` lấy ngày không kèm thời gian, trong khi `java.time.LocalDateTime` lấy cả ngày và giờ, không kèm múi giờ. Nếu bạn cần múi giờ, có `java.time.ZonedDateTime`. Đối với chỉ giờ, có `java.time.LocalTime`.

Về các lựa chọn khác, thư viện như Joda-Time tồn tại trước Java 8, và một số dự án cũ có thể vẫn sử dụng nó. Nhưng kể từ khi giới thiệu gói `java.time` của Java 8, nó được coi là tiêu chuẩn, và với những lý do chính đáng. Nó toàn diện và phù hợp với hệ thống lịch ISO-8601.

Từ góc độ triển khai, các phương thức `now()` trong các lớp của `java.time` lấy ngày/giờ hiện tại từ đồng hồ hệ thống, là quan điểm của máy tính về thời gian hiện tại, liên kết với thế giới thực thông qua cài đặt hệ thống và đồng bộ hóa thời gian internet.

## Xem Thêm:

- Tài liệu chính thức của gói `java.time`: [https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- Tiêu chuẩn Ngày và Giờ của ISO 8601: [https://www.iso.org/iso-8601-date-and-time-format.html](https://www.iso.org/iso-8601-date-and-time-format.html)
- Đối với quản lý ngày của Java kiểu cũ, xem lớp `Calendar`: [https://docs.oracle.com/javase/7/docs/api/java/util/Calendar.html](https://docs.oracle.com/javase/7/docs/api/java/util/Calendar.html)
