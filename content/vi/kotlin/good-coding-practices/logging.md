---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:10.689371-07:00
description: "L\xE0m Th\u1EBF N\xE0o: Trong Kotlin, vi\u1EC7c logging c\xF3 th\u1EC3\
  \ \u0111\u01B0\u1EE3c th\u1EF1c hi\u1EC7n b\u1EB1ng c\xE1ch s\u1EED d\u1EE5ng h\xE0\
  m `println()` \u0111\u01B0\u1EE3c t\xEDch h\u1EE3p s\u1EB5n cho c\xE1c tr\u01B0\u1EDD\
  ng h\u1EE3p \u0111\u01A1n gi\u1EA3n, ho\u1EB7c v\u1EDBi\u2026"
lastmod: '2024-03-13T22:44:36.610216-06:00'
model: gpt-4-0125-preview
summary: "Trong Kotlin, vi\u1EC7c logging c\xF3 th\u1EC3 \u0111\u01B0\u1EE3c th\u1EF1\
  c hi\u1EC7n b\u1EB1ng c\xE1ch s\u1EED d\u1EE5ng h\xE0m `println()` \u0111\u01B0\u1EE3\
  c t\xEDch h\u1EE3p s\u1EB5n cho c\xE1c tr\u01B0\u1EDDng h\u1EE3p \u0111\u01A1n gi\u1EA3\
  n, ho\u1EB7c v\u1EDBi c\xE1c th\u01B0 vi\u1EC7n ph\u1EE9c t\u1EA1p h\u01A1n nh\u01B0\
  \ SLF4J k\xE8m theo Logback ho\u1EB7c Log4j cho nh\u1EEFng nhu c\u1EA7u n\xE2ng\
  \ cao."
title: Ghi log
weight: 17
---

## Làm Thế Nào:
Trong Kotlin, việc logging có thể được thực hiện bằng cách sử dụng hàm `println()` được tích hợp sẵn cho các trường hợp đơn giản, hoặc với các thư viện phức tạp hơn như SLF4J kèm theo Logback hoặc Log4j cho những nhu cầu nâng cao.

Dưới đây là một ví dụ cơ bản sử dụng `println()`:

```Kotlin
fun main() {
    println("Thông điệp log đơn giản: Ứng dụng đã được khởi động.")
    // ... một số logic ứng dụng ở đây ...
    try {
        // Mô phỏng một lỗi
        throw Exception("Lỗi được mô phỏng")
    } catch (e: Exception) {
        println("Thông điệp log lỗi: " + e.message)
    }
}
```

Kết quả:
```
Thông điệp log đơn giản: Ứng dụng đã được khởi động.
Thông điệp log lỗi: Lỗi được mô phỏng
```

Và đây là một đoạn mã mẫu sử dụng SLF4J với Logback được cấu hình:

```Kotlin
import org.slf4j.LoggerFactory

private val logger = LoggerFactory.getLogger("MyAppLogger")

fun main() {
    logger.info("Thông điệp log có cấu trúc: Ứng dụng được khởi chạy.")
    // ... một số logic ứng dụng ở đây ...
    try {
        // Mô phỏng một lỗi
        throw Exception("Lỗi được mô phỏng")
    } catch (e: Exception) {
        logger.error("Log lỗi có cấu trúc: ", e)
    }
}
```

Giả sử việc cấu hình Logback phù hợp, kết quả sẽ được định dạng và có thể trông giống như sau khi được ghi vào tệp log:
```
[INFO] - 2023-03-29 14:15:42 - MyAppLogger - Thông điệp log có cấu trúc: Ứng dụng được khởi chạy.
[ERROR] - 2023-03-29 14:15:43 - MyAppLogger - Log lỗi có cấu trúc: 
java.lang.Exception: Lỗi được mô phỏng
   at com.myapp.Main.main(Main.kt:10)
```

## Sâu Hơn
Lịch sử, việc logging trong phần mềm phát triển cùng với sự phức tạp ngày càng tăng của các ứng dụng và hệ thống. Các câu lệnh in đơn giản đã đủ cho những ngày đầu, khi các chương trình thường được chạy và gỡ lỗi bởi chính những nhà phát triển. Nhưng khi các hệ thống được kết nối mạng và chạy trong các môi trường khác nhau trên các người dùng khác nhau, một hệ thống logging vững chắc và lâu dài trở nên quan trọng.

Trước khi Kotlin trở nên phổ biến, các nhà phát triển Java đã rộng rãi áp dụng các thư viện như Log4j và sau đó là SLF4J. Những thư viện này đã truyền cảm hứng cho những phương pháp tương tự trong Kotlin, tận dụng khả năng tương thích của Kotlin với các thư viện Java. SLF4J hoạt động như một lớp trừu tượng, cho phép việc triển khai logging thực tế có thể được thay thế—thường Logback hoặc Log4j2 là những lựa chọn được ưu tiên.

Kotlin cũng cho phép giải pháp logging đa nền tảng hoạt động trên JVM, JavaScript, và Native, ví dụ, thông qua cơ chế `expect`/`actual`, che giấu đi những triển khai cụ thể theo nền tảng.

So với các thư viện logging chuyên dụng, println vẫn là hình thức logging đơn giản nhất bởi vì nó không yêu cầu cài đặt bổ sung hoặc phụ thuộc; tuy nhiên, nó thường không phù hợp cho các ứng dụng sản xuất do thiếu các tính năng như mức độ log, quay vòng log, và định dạng có cấu trúc.

Các tính năng phổ biến khác của các khung logging nâng cao bao gồm:

- Các mức log (DEBUG, INFO, WARN, ERROR, v.v.) để phân loại mức độ cấp bách của thông điệp log.
- Xuất ra các nguồn khác nhau, như console, tệp, cơ sở dữ liệu, hoặc dịch vụ mạng.
- Tự động quay vòng và chính sách giữ log.
- Hỗ trợ tracing phân tán cho kiến trúc microservices.
- Logging có cấu trúc sử dụng các định dạng như JSON, tích hợp tốt với các hệ thống phân tích log.

Công cụ và các tính năng này rất quan trọng cho việc duy trì một hệ thống đáng tin cậy, có thể quan sát đặc biệt trong môi trường phức tạp, phân tán, hoặc có quy mô lớn.

## Xem Thêm
Để tìm hiểu thêm và có cái nhìn sâu sắc hơn về logging trong Kotlin, hãy kiểm tra:

- SLF4J (Simple Logging Facade for Java) [http://www.slf4j.org/](http://www.slf4j.org/)
- Logback, người kế nhiệm của Log4j [http://logback.qos.ch/](http://logback.qos.ch/)
- Log4j 2 [https://logging.apache.org/log4j/2.x/](https://logging.apache.org/log4j/2.x/)
- Tài liệu đa nền tảng Kotlin về các khai báo 'expect' và 'actual': [https://kotlinlang.org/docs/multiplatform.html](https://kotlinlang.org/docs/multiplatform.html)
- Hướng dẫn về logging có cấu trúc trong Kotlin: [https://ktor.io/docs/logging.html](https://ktor.io/docs/logging.html)
