---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:38.849406-07:00
description: "Chuy\u1EC3n \u0111\u1ED5i ng\xE0y th\xE0nh chu\u1ED7i ngh\u0129a l\xE0\
  \ bi\u1EC3u di\u1EC5n m\u1ED9t \u0111\u1ED1i t\u01B0\u1EE3ng ng\xE0y d\u01B0\u1EDB\
  i d\u1EA1ng v\u0103n b\u1EA3n d\u1EC5 \u0111\u1ECDc theo m\u1ED9t m\u1EABu c\u1EE5\
  \ th\u1EC3. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\
  \u1EC3 hi\u1EC3n\u2026"
lastmod: '2024-03-11T00:14:09.770593-06:00'
model: gpt-4-0125-preview
summary: "Chuy\u1EC3n \u0111\u1ED5i ng\xE0y th\xE0nh chu\u1ED7i ngh\u0129a l\xE0 bi\u1EC3\
  u di\u1EC5n m\u1ED9t \u0111\u1ED1i t\u01B0\u1EE3ng ng\xE0y d\u01B0\u1EDBi d\u1EA1\
  ng v\u0103n b\u1EA3n d\u1EC5 \u0111\u1ECDc theo m\u1ED9t m\u1EABu c\u1EE5 th\u1EC3\
  . L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3 hi\u1EC3\
  n\u2026"
title: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i"
---

{{< edit_this_page >}}

## Gì và Tại sao?

Chuyển đổi ngày thành chuỗi nghĩa là biểu diễn một đối tượng ngày dưới dạng văn bản dễ đọc theo một mẫu cụ thể. Lập trình viên thực hiện việc này để hiển thị ngày cho người dùng hoặc để chuẩn hóa chúng cho việc lưu trữ và giao tiếp mạng trong một định dạng thân thiện với con người.

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
