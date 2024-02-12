---
title:                "Sử dụng vỏ tương tác (REPL)"
aliases:
- /vi/kotlin/using-an-interactive-shell-repl.md
date:                  2024-01-28T22:09:28.330461-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng vỏ tương tác (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/kotlin/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
REPL (Read-Eval-Print Loop) là môi trường lập trình máy tính tương tác, đơn giản. Các lập trình viên sử dụng nó để thử nghiệm mã nhanh chóng, kiểm tra đoạn mã, hoặc học cú pháp của một ngôn ngữ mà không cần tạo một ứng dụng đầy đủ.

## Cách thực hiện:
Khởi chạy REPL của Kotlin rất dễ dàng. Mở terminal của bạn và gõ `kotlinc`. Bạn sẽ nhập vào shell Kotlin. Hãy thử định nghĩa một biến và in giá trị của nó:

```kotlin
Chào mừng bạn đến với Kotlin phiên bản 1.7.10 (JRE 1.8.0_292-b10)
Gõ :help để nhận trợ giúp, :quit để thoát
>>> val greeting = "Xin chào, Kotlin REPL!"
>>> println(greeting)
Xin chào, Kotlin REPL!
```

## Tìm hiểu sâu
REPL của Kotlin được giới thiệu cùng với ngôn ngữ để khuyến khích thử nghiệm. Nó tương tự như shell tương tác của Python nhưng được tùy chỉnh cho cú pháp và đặc điểm riêng của Kotlin. Các lựa chọn khác? Môi trường tương tác trong các IDE như IntelliJ IDEA, và sân chơi Kotlin trực tuyến. REPL hoạt động bằng cách biên dịch mã ngay lập tức, cung cấp phản hồi tức thì – điều cực kỳ quan trọng cho việc học và gỡ lỗi.

## Xem thêm
- Tài liệu Kotlin về REPL: [https://kotlinlang.org/docs/command-line.html#run-the-repl](https://kotlinlang.org/docs/command-line.html#run-the-repl)
- Thử Kotlin trên trình duyệt: [https://play.kotlinlang.org](https://play.kotlinlang.org)
- Plugin JetBrains Kotlin Playground cho IntelliJ IDEA.
