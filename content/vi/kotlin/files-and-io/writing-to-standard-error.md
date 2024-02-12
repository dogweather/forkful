---
title:                "Ghi vào lỗi chuẩn"
aliases: - /vi/kotlin/writing-to-standard-error.md
date:                  2024-01-28T22:13:45.354191-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi vào lỗi chuẩn"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Lỗi tiêu chuẩn (stderr) là một luồng nơi mà chương trình viết các thông báo lỗi của mình. Lập trình viên sử dụng nó để tách biệt nhật ký lỗi khỏi đầu ra tiêu chuẩn (stdout) nhằm gỡ lỗi hiệu quả hơn và tổ chức nhật ký một cách thuận tiện.

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
