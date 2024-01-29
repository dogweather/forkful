---
title:                "Sử dụng bộ gỡ lỗi"
date:                  2024-01-28T22:09:34.727757-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng bộ gỡ lỗi"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/using-a-debugger.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Sử dụng một trình gỡ lỗi giống như việc có một thiết bị GPS trong rừng mã; nó hướng dẫn bạn đến nguồn gốc của vấn đề. Lập trình viên sử dụng trình gỡ lỗi để bước qua từng dòng mã của họ, kiểm tra các biến và hiểu luồng, giúp việc bắt lỗi và tối ưu hoá hiệu suất trở nên dễ dàng hơn.

## Làm thế nào:
Go có một công cụ tích hợp sẵn để gỡ lỗi gọi là Delve (`dlv`). Để bắt đầu, hãy cài đặt Delve, viết một chương trình Go đơn giản, rồi chạy nó qua trình gỡ lỗi.

```Go
// Đầu tiên, cài đặt Delve
// go get -u github.com/go-delve/delve/cmd/dlv

// Chương trình Go ví dụ, lưu thành main.go
package main

import "fmt"

func main() {
    message := "Gỡ lỗi với Delve!"
    fmt.Println(message)
}

// Chạy chương trình của bạn với Delve
// dlv debug

// Một số lệnh cơ bản của Delve:
// (dlv) break main.main // đặt một điểm dừng tại hàm main
// (dlv) continue // chạy đến điểm dừng hoặc khi chương trình kết thúc
// (dlv) step // bước qua từng lệnh trong chương trình
// (dlv) print message // in giá trị hiện tại của biến 'message'
// (dlv) quit // thoát Delve
```

Chạy `dlv debug` khởi đầu một phiên gỡ lỗi. Một khi bạn đạt đến một điểm dừng bạn đã đặt, bạn có thể bước qua chương trình của mình và xem điều gì đang diễn ra bên dưới.

## Sâu hơn
Trong lịch sử, lập trình viên Go đã sử dụng nhiều công cụ để gỡ lỗi như GDB (GNU Debugger) nhưng đã đối mặt với thách thức bởi vì GDB không được thiết kế riêng cho thời gian chạy và goroutines của Go. Delve đã giải cứu với sự hỗ trợ tốt hơn cho các tính năng độc đáo của Go.

Có các phương án thay thế cho Delve như `go-dbg`, và thậm chí là hỗ trợ gỡ lỗi tích hợp sẵn trong các môi trường phát triển tích hợp (IDE) như Visual Studio Code và GoLand, đó bao quanh Delve để mang lại trải nghiệm thân thiện hơn với người dùng.

Về mặt thực thi, Delve hoạt động sử dụng các gói `runtime` và `debug/gosym`, trong số những người khác, để truy cập và giải thích các biểu tượng và thông tin thời gian chạy của chương trình Go. Nó liên tục được cập nhật để theo kịp với các tính năng và phiên bản ngôn ngữ mới.

## Xem thêm
- Kho chính thức của Delve: https://github.com/go-delve/delve
- Hướng dẫn sử dụng Gỡ lỗi Go bởi Nhóm Go: https://golang.org/doc/gdb
- Gỡ lỗi Go với Visual Studio Code: https://code.visualstudio.com/docs/languages/go#_debugging
