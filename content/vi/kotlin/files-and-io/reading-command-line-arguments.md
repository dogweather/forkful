---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:33.007358-07:00
description: "L\xE0m th\u1EBF n\xE0o: ."
lastmod: '2024-03-13T22:44:36.621886-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh"
weight: 23
---

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
