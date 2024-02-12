---
title:                "Đọc các đối số dòng lệnh"
aliases: - /vi/kotlin/reading-command-line-arguments.md
date:                  2024-01-28T22:05:33.007358-07:00
model:                 gpt-4-0125-preview
simple_title:         "Đọc các đối số dòng lệnh"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/reading-command-line-arguments.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
