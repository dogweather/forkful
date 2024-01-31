---
title:                "Nội suy chuỗi ký tự"
date:                  2024-01-28T22:02:20.990610-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nội suy chuỗi ký tự"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái Gì & Tại Sao?

Nội suy chuỗi cho phép bạn nhúng biến vào trong chuỗi. Nó rất tiện lợi cho việc tạo thông báo, định dạng dữ liệu và xây dựng truy vấn SQL mà không cần nhiều dấu cộng và dấu ngoặc kép.

## Cách thực hiện:

Trong Go, bạn sử dụng gói `fmt` để nội suy chuỗi.

```Go
package main

import (
    "fmt"
)

func main() {
    name := "Morgan"
    age := 28
    message := fmt.Sprintf("Xin chào, tên tôi là %s và tôi %d tuổi.", name, age)
    fmt.Println(message)
}

// Kết quả: Xin chào, tên tôi là Morgan và tôi 28 tuổi.
```

Sử dụng `%s` cho chuỗi, `%d` cho số nguyên, `%f` cho số thực. Có nhiều đại từ chỉ định khác cho các kiểu dữ liệu khác.

## Sâu hơn:

Nội suy chuỗi đã là tính năng cơ bản trong nhiều ngôn ngữ - Python, Ruby, và hơn thế nữa. Trong Go, nó không phải là một phần của ngôn ngữ per se nhưng được cung cấp qua gói `fmt`. Cách tiếp cận này cho bạn kiểm soát và an toàn hơn, đặc biệt là với các đại từ chỉ định cụ thể theo kiểu.

Có phương án thay thế? Có - ngoài `fmt.Sprintf`, còn có `fmt.Fprintf` để viết vào bất kỳ writer nào, và `fmt.Printf` để in trực tiếp. Trước những ngày của Go 1.10, mọi người đã thấy kết nối chuỗi với `+` hoặc sử dụng `bytes.Buffer`. Những phương pháp này vẫn còn hợp lệ nhưng kém tiện lợi hơn.

Chi tiết triển khai? Gói `fmt` sử dụng phản xạ để xử lý định dạng dựa trên các đại từ chỉ định và kiểu của biến. Nó hiệu quả nhưng nhớ rằng sử dụng sai đại từ chỉ định cho một kiểu có thể dẫn đến lỗi thời gian chạy.

## Xem thêm:

- Tài liệu gói `fmt` của Go: https://pkg.go.dev/fmt
- Cách tiếp cận của Go by Example về định dạng chuỗi: https://gobyexample.com/string-formatting
- Một bài đăng trên blog về các chiến lược nối chuỗi trong Go: https://blog.golang.org/strings
