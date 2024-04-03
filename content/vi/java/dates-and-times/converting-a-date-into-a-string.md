---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:38.849406-07:00
description: "L\xE0m th\u1EBF n\xE0o: Java l\xE0m cho vi\u1EC7c chuy\u1EC3n \u0111\
  \u1ED5i ng\xE0y th\xE0nh chu\u1ED7i tr\u1EDF n\xEAn d\u1EC5 d\xE0ng. L\u1EDBp `java.time.format.DateTimeFormatter`\
  \ l\xE0 l\u1EF1a ch\u1ECDn h\xE0ng \u0111\u1EA7u c\u1EE7a b\u1EA1n. D\u01B0\u1EDB\
  i\u2026"
lastmod: '2024-03-13T22:44:36.504365-06:00'
model: gpt-4-0125-preview
summary: "Java l\xE0m cho vi\u1EC7c chuy\u1EC3n \u0111\u1ED5i ng\xE0y th\xE0nh chu\u1ED7\
  i tr\u1EDF n\xEAn d\u1EC5 d\xE0ng."
title: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i"
weight: 28
---

## Làm thế nào:
Java làm cho việc chuyển đổi ngày thành chuỗi trở nên dễ dàng. Lớp `java.time.format.DateTimeFormatter` là lựa chọn hàng đầu của bạn. Dưới đây là một ví dụ về mã:

```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class DateToStringExample {
    public static void main(String[] args) {
        LocalDate date = LocalDate.now(); // Ngày hôm nay
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
        String dateString = date.format(formatter);
        System.out.println(dateString); // Kết quả có thể là: 20/03/2023, ví dụ
    }
}
```

## Sâu hơn nữa
Trong lịch sử, Java đã sử dụng `SimpleDateFormat` từ gói `java.text`, nhưng nó không đảm bảo an toàn cho luồng và dẫn đến lỗi. Với Java 8, gói `java.time` đã mang lại các lớp ngày-giờ không thể thay đổi và an toàn cho luồng. `DateTimeFormatter` là một phần của gói hiện đại này.

Có những lựa chọn thay thế như `FastDateFormat` từ Apache Commons và `DateUtils` từ các thư viện khác nhau. Tuy nhiên, hầu hết các nhà phát triển Java vẫn tuân theo thư viện chuẩn, vốn mạnh mẽ và đa năng.

Khi định dạng, `DateTimeFormatter` sử dụng các mẫu `yyyy` cho năm, `MM` cho tháng và `dd` cho ngày. Nó có thể xử lý các mẫu phức tạp, thậm chí là các mẫu cụ thể cho địa phương, với phương thức `ofPattern` của mình. Cũng đáng chú ý là `DateTimeFormatter` không thể thay đổi và an toàn cho luồng, vì vậy bạn có thể sử dụng cùng một thực thể formatter trên nhiều luồng mà không cần đau đầu về đồng bộ hóa.

## Xem thêm
- Tài liệu Java chính thức của Oracle dành cho `DateTimeFormatter`: https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html
- Để biết thêm về các mẫu ngày và thời gian: https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html#patterns
- Tổng quan về Ngày và Thời gian trong Java 8: https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html
