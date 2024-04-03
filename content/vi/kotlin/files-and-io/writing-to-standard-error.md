---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:45.354191-07:00
description: "L\u1ED7i ti\xEAu chu\u1EA9n (stderr) l\xE0 m\u1ED9t lu\u1ED3ng n\u01A1\
  i m\xE0 ch\u01B0\u01A1ng tr\xECnh vi\u1EBFt c\xE1c th\xF4ng b\xE1o l\u1ED7i c\u1EE7\
  a m\xECnh. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng n\xF3 \u0111\u1EC3 t\xE1\
  ch bi\u1EC7t nh\u1EADt k\xFD l\u1ED7i kh\u1ECFi \u0111\u1EA7u ra\u2026"
lastmod: '2024-03-13T22:44:36.623141-06:00'
model: gpt-4-0125-preview
summary: "L\u1ED7i ti\xEAu chu\u1EA9n (stderr) l\xE0 m\u1ED9t lu\u1ED3ng n\u01A1i\
  \ m\xE0 ch\u01B0\u01A1ng tr\xECnh vi\u1EBFt c\xE1c th\xF4ng b\xE1o l\u1ED7i c\u1EE7\
  a m\xECnh."
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
weight: 25
---

## Cách làm:
Dưới đây là một đoạn mã Kotlin đơn giản để in ra lỗi tiêu chuẩn:

```kotlin
fun main() {
    System.err.println("Ôi, đã xảy ra lỗi.")
}
```

Và đầu ra trong bảng điều khiển của bạn sẽ trông như thế này (phong cách có thể thay đổi tùy theo terminal):

```
Ôi, đã xảy ra lỗi.
```

## Sâu hơn nữa
Ban đầu trong các hệ thống giống Unix, lý do cho stderr rất rõ ràng: stderr cho phép các thông báo lỗi được gửi đến màn hình hoặc một tệp khác khác với đầu ra bình thường. Điều này giúp phân biệt dữ liệu bình thường với các thông báo lỗi, đặc biệt hữu ích khi đầu ra được đưa đi nơi khác.

Các phương án thay thế cho `System.err.println` bao gồm việc sử dụng một khung làm việc nhật ký như Logback hoặc log4j, có nhiều quyền kiểm soát và tùy chọn như cấp độ nhật ký và xuất ra tệp.

`System.err` trong Kotlin được kế thừa từ lớp `System` của Java, tương tự như `System.out` cho đầu ra tiêu chuẩn, cả hai đều là các đối tượng PrintStream. Theo mặc định, `System.err` in ra bảng điều khiển. Tuy nhiên, nó có thể được chuyển hướng để viết vào một tệp hoặc một luồng đầu ra khác nhau.

## Xem thêm
- Tài liệu Kotlin về I/O cơ bản: https://kotlinlang.org/docs/basic-io.html
- Thông tin về luồng tiêu chuẩn Unix: https://en.wikipedia.org/wiki/Standard_streams
- Logback, một khung làm việc nhật ký phổ biến: http://logback.qos.ch/
- Apache log4j, một khung làm việc nhật ký khác: https://logging.apache.org/log4j/2.x/
