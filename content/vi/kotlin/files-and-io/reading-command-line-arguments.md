---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:33.007358-07:00
description: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh c\xF3 ngh\u0129\
  a l\xE0 l\u1EA5y d\u1EEF li\u1EC7u \u0111\u01B0\u1EE3c truy\u1EC1n t\u1EDBi ch\u01B0\
  \u01A1ng tr\xECnh c\u1EE7a b\u1EA1n khi n\xF3 kh\u1EDFi \u0111\u1ED9ng. C\xE1c l\u1EAD\
  p tr\xECnh vi\xEAn c\u1EA7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 cho ph\xE9p ng\u01B0\
  \u1EDDi\u2026"
lastmod: 2024-02-19 22:04:55.793465
model: gpt-4-0125-preview
summary: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh c\xF3 ngh\u0129\
  a l\xE0 l\u1EA5y d\u1EEF li\u1EC7u \u0111\u01B0\u1EE3c truy\u1EC1n t\u1EDBi ch\u01B0\
  \u01A1ng tr\xECnh c\u1EE7a b\u1EA1n khi n\xF3 kh\u1EDFi \u0111\u1ED9ng. C\xE1c l\u1EAD\
  p tr\xECnh vi\xEAn c\u1EA7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 cho ph\xE9p ng\u01B0\
  \u1EDDi\u2026"
title: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh"
---

{{< edit_this_page >}}

## Gì và Tại sao?
Đọc các đối số dòng lệnh có nghĩa là lấy dữ liệu được truyền tới chương trình của bạn khi nó khởi động. Các lập trình viên cần điều này để cho phép người dùng tùy chỉnh hành vi của chương trình mà không cần thay đổi mã.

## Làm thế nào:

```kotlin
fun main(args: Array<String>) {
    if (args.isNotEmpty()) {
        println("Xin chào, ${args[0]}!")
    } else {
        println("Xin chào, người lạ!")
    }
}

// Đầu ra mẫu nếu truyền 'Kotlinista' làm đối số:
// Xin chào, Kotlinista!
```

Trong đoạn mã trên, `args` là một mảng chứa các đối số dòng lệnh. Hàm `main` kiểm tra xem chúng ta có nhận được đối số nào không, và chào hỏi tương ứng.

## Sâu hơn
Khái niệm về các đối số dòng lệnh cũ như trái đất; nó đã là một phần của lập trình từ thời sơ khai - hoặc ít nhất là kể từ khi tạo ra các thiết bị đầu cuối tương tác. Trong bối cảnh của Kotlin, chạy trên JVM, các đối số dòng lệnh hoạt động tương tự như Java.

Các ngôn ngữ khác cung cấp phương tiện tương tự, như `argv` trong Python hoặc `$argc` và `$argv` trong PHP. Cách tiếp cận của Kotlin giữ cho mọi thứ đơn giản - hàm `main` chỉ cần lấy một `Array<String>`.

Về chi tiết triển khai, nhớ rằng chỉ số của mảng bắt đầu từ không. `args[0]` là đối số đầu tiên, `args[1]` là đối số thứ hai, và cứ thế tiếp tục. Ngoài ra, hãy ghi nhớ rằng nếu bạn đang xây dựng một ứng dụng phức tạp cần phân tích các lệnh một cách linh hoạt hơn, bạn có thể muốn tìm hiểu một thư viện chuyên dụng như kotlinx-cli.

## Xem thêm
- [Tài liệu Chính Thức của Kotlin về Ứng Dụng Dòng Lệnh](https://kotlinlang.org/docs/command-line.html)
- [kotlinx-cli trên GitHub](https://github.com/Kotlin/kotlinx-cli)
