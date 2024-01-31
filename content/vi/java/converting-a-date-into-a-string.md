---
title:                "Chuyển đổi một ngày thành chuỗi"
date:                  2024-01-28T21:57:38.849406-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi một ngày thành chuỗi"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/java/converting-a-date-into-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
