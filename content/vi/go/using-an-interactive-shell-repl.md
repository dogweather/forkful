---
title:                "Sử dụng vỏ tương tác (REPL)"
date:                  2024-01-28T22:09:23.229264-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng vỏ tương tác (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/using-an-interactive-shell-repl.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
REPL (Read-Eval-Print Loop - Vòng lặp Đọc-Đánh giá-In) cho phép bạn tương tác trực tiếp với code; nó đọc đầu vào, đánh giá, in kết quả và lặp lại. Các lập trình viên sử dụng nó để thử đoạn mã, gỡ lỗi và học ngôn ngữ mới theo thời gian thực.

## Cách thực hiện:
Go không bao gồm một REPL tích hợp sẵn, nhưng bạn có thể sử dụng các công cụ của bên thứ ba. Một công cụ phổ biến là `gore`:

```go
// Cài đặt gore sử dụng
$ go install github.com/motemen/gore/cmd/gore@latest

// Chạy gore
$ gore
gore version 0.5.0  :help for help
gore> :import fmt
gore> fmt.Println("Xin chào, Go REPL!")
Xin chào, Go REPL!
nil
```

## Sâu hơn nữa
Ban đầu được phát triển cho Lisp, REPL phổ biến trong các ngôn ngữ động như Python hoặc Ruby. Go, khi là một ngôn ngữ định kiểu tĩnh, không bao gồm một cái ngay lập tức. Các lựa chọn thay thế cho `gore` bao gồm `go-pry` và `yaegi`. Những công cụ này giải thích mã Go, cho phép bạn khám phá và xác thực ý tưởng nhanh chóng mà không cần biên dịch một ứng dụng đầy đủ. Chúng đặc biệt hữu ích cho người mới bắt đầu và trong các ngữ cảnh giáo dục, nơi mà trọng tâm là học và thử nghiệm.

## Xem thêm
- `gore`: [https://github.com/motemen/gore](https://github.com/motemen/gore)
- `go-pry`: [https://github.com/d4l3k/go-pry](https://github.com/d4l3k/go-pry) 
- `yaegi`: [https://github.com/traefik/yaegi](https://github.com/traefik/yaegi)
