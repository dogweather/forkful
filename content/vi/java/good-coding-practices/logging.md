---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:13.090044-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1\
  ch \u0111\u01A1n gi\u1EA3n \u0111\u1EC3 b\u1EAFt \u0111\u1EA7u v\u1EDBi vi\u1EC7\
  c ghi nh\u1EADt k\xFD trong Java s\u1EED d\u1EE5ng g\xF3i `java.util.logging` c\xF3\
  \ s\u1EB5n."
lastmod: '2024-03-13T22:44:36.497899-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch \u0111\u01A1n gi\u1EA3n \u0111\u1EC3\
  \ b\u1EAFt \u0111\u1EA7u v\u1EDBi vi\u1EC7c ghi nh\u1EADt k\xFD trong Java s\u1EED\
  \ d\u1EE5ng g\xF3i `java.util.logging` c\xF3 s\u1EB5n."
title: Ghi log
weight: 17
---

## Cách thực hiện:
Dưới đây là cách đơn giản để bắt đầu với việc ghi nhật ký trong Java sử dụng gói `java.util.logging` có sẵn.

```java
import java.util.logging.Logger;
import java.util.logging.Level;

public class AppLogging {
    private final static Logger LOGGER = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);

    public static void main(String[] args) {
        LOGGER.info("Ghi một thông điệp cấp độ INFO");

        try {
            int division = 10 / 0;
        } catch (ArithmeticException e) {
            LOGGER.log(Level.SEVERE, "Xảy ra ngoại lệ", e);
        }
    }
}
```

Điều này sẽ tạo ra đầu ra như sau:

```
Jul 03, 2023 2:00:00 CH AppLogging main
INFO: Ghi một thông điệp cấp độ INFO
Jul 03, 2023 2:00:00 CH AppLogging main
SEVERE: Xảy ra ngoại lệ
java.lang.ArithmeticException: / cho zero
    tại AppLogging.main(AppLogging.java:10)
```

## Sâu hơn
Việc ghi nhật ký trong Java đã phát triển khá nhiều. Trước đây, việc ghi nhật ký thường được thực hiện một cách tùy tiện với các output hệ thống và cơ chế tự viết. Tuy nhiên, nhu cầu về sự chuẩn hóa đã dẫn đến việc ra đời các API ghi nhật ký như `Log4j` và `SLF4J`. Gói `java.util.logging` được giới thiệu trong JDK 1.4, cung cấp một cách chuẩn hóa để ghi nhật ký.

Các lựa chọn thay thế cho `java.util.logging` (JUL) bao gồm Log4j 2 và SLF4J. Trong khi JUL được xây dựng sẵn trong Java và do đó không cần thêm phụ thuộc, cả Log4j 2 và SLF4J đều cung cấp những tính năng nâng cao như kiểm soát cấu hình ghi nhật ký một cách chi tiết hơn, ghi nhật ký bất đồng bộ và hiệu suất tốt hơn.

Về mặt triển khai, việc ghi nhật ký có thể là đồng bộ, nơi mỗi thông điệp nhật ký được xử lý trong luồng đã tạo ra nó, hoặc là bất đồng bộ, nơi các thông điệp được chuyển giao cho một luồng riêng biệt. Ghi nhật ký bất đồng bộ có thể cải thiện hiệu suất nhưng giới thiệu độ phức tạp khi người ta phải xử lý đồng thời và đảm bảo rằng các thông điệp nhật ký không bị mất khi ứng dụng bị sập.

## Xem thêm
- [Log4j 2](https://logging.apache.org/log4j/2.x/)
- [SLF4J](http://www.slf4j.org/)
- [Tổng quan về ghi nhật ký chính thức của Oracle](https://docs.oracle.com/javase/8/docs/technotes/guides/logging/overview.html)
- [Hướng dẫn về java.util.logging](https://www.vogella.com/tutorials/Logging/article.html)
