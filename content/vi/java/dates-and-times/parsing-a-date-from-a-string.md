---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:25.208298-07:00
description: "L\xE0m Th\u1EBF N\xE0o: Java c\xF3 m\u1ED9t l\u1EDBp `java.time.format.DateTimeFormatter`\
  \ d\xE0nh cho lo\u1EA1i c\xF4ng vi\u1EC7c n\xE0y. D\u01B0\u1EDBi \u0111\xE2y l\xE0\
  \ c\xE1ch b\u1EA1n s\u1EED d\u1EE5ng n\xF3."
lastmod: '2024-03-13T22:44:36.501758-06:00'
model: gpt-4-0125-preview
summary: "Java c\xF3 m\u1ED9t l\u1EDBp `java.time.format.DateTimeFormatter` d\xE0\
  nh cho lo\u1EA1i c\xF4ng vi\u1EC7c n\xE0y."
title: "Ph\xE2n t\xEDch ng\xE0y t\u1EEB chu\u1ED7i k\xED t\u1EF1"
weight: 30
---

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
