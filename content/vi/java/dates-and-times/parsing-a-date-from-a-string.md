---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:25.208298-07:00
description: "Ph\xE2n t\xEDch c\xFA ph\xE1p c\u1EE7a m\u1ED9t ng\xE0y t\u1EEB m\u1ED9\
  t chu\u1ED7i c\xF3 ngh\u0129a l\xE0 chuy\u1EC3n v\u0103n b\u1EA3n th\xE0nh m\u1ED9\
  t \u0111\u1ED1i t\u01B0\u1EE3ng Date m\xE0 m\u1ED9t ch\u01B0\u01A1ng tr\xECnh c\xF3\
  \ th\u1EC3 s\u1EED d\u1EE5ng. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m\u2026"
lastmod: '2024-03-13T22:44:36.501758-06:00'
model: gpt-4-0125-preview
summary: "Ph\xE2n t\xEDch c\xFA ph\xE1p c\u1EE7a m\u1ED9t ng\xE0y t\u1EEB m\u1ED9\
  t chu\u1ED7i c\xF3 ngh\u0129a l\xE0 chuy\u1EC3n v\u0103n b\u1EA3n th\xE0nh m\u1ED9\
  t \u0111\u1ED1i t\u01B0\u1EE3ng Date m\xE0 m\u1ED9t ch\u01B0\u01A1ng tr\xECnh c\xF3\
  \ th\u1EC3 s\u1EED d\u1EE5ng. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m\u2026"
title: "Ph\xE2n t\xEDch ng\xE0y t\u1EEB chu\u1ED7i k\xED t\u1EF1"
---

{{< edit_this_page >}}

## Cái Gì & Tại Sao?

Phân tích cú pháp của một ngày từ một chuỗi có nghĩa là chuyển văn bản thành một đối tượng Date mà một chương trình có thể sử dụng. Các lập trình viên làm điều này để hiểu người dùng nhập vào hoặc dữ liệu được lưu trữ ở định dạng dễ đọc.

## Làm Thế Nào:

Java có một lớp `java.time.format.DateTimeFormatter` dành cho loại công việc này. Dưới đây là cách bạn sử dụng nó.

```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;

public class DateParser {

    public static void main(String[] args) {
        String dateString = "2023-03-15";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd");

        try {
            LocalDate date = LocalDate.parse(dateString, formatter);
            System.out.println("Ngày đã phân tích cú pháp: " + date);
        } catch (DateTimeParseException e) {
            System.err.println("Rất tiếc, ngày không đúng định dạng!");
        }
    }
}
```

Chạy đoạn mã nhỏ này, và bạn sẽ thấy:

```
Ngày đã phân tích cú pháp: 2023-03-15
```

## Sâu Hơn Nữa

Trước khi `java.time` đẹp trai nhập cuộc với Java 8 vào năm 2014, mọi người đã quen với việc sử dụng `java.util.Date` và `SimpleDateFormat`. Những cái cũ không chỉ thù địch với luồng mà còn là một cơn đau đầu khi sử dụng với các quirk về múi giờ của chúng.

Ngày nay, `java.time` là cái mới. Nó an toàn với luồng, bất biến (không thay đổi một cách lén lút), và rõ ràng hơn về ý định. Thêm vào đó, bạn có thể chọn từ một bộ các bộ định dạng đã định nghĩa sẵn hoặc tự tạo của mình với các mẫu.

Các lựa chọn thay thế, bạn hỏi? Thư viện như Joda-Time đã mở đường, nhưng kể từ khi java.time mượn nặng nề từ ý tưởng của nó, hầu hết đã treo mũ ủng hộ thư viện chuẩn.

Dưới capô, phân tích cú pháp với `DateTimeFormatter` thực hiện một số công việc nặng nhọc. Nó kiểm tra các mẫu, xác thực đầu vào, xử lý ngoại lệ và cung cấp một `LocalDate`, `LocalTime` hoặc thậm chí `ZonedDateTime` tùy thuộc vào nhu cầu của bạn.

## Xem Thêm

- Tài liệu Java chính thức cho `java.time.format.DateTimeFormatter`: https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html
- Các hướng dẫn Java của Oracle, bao gồm ngày và giờ: https://docs.oracle.com/javase/tutorial/datetime/
