---
title:                "Sắp xếp mã thành các hàm"
date:                  2024-01-28T22:03:28.727741-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sắp xếp mã thành các hàm"
programming_language: "Go"
category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Việc tổ chức code thành các hàm là việc phân chia code của bạn thành các phần có thể tái sử dụng. Nó làm cho code của bạn sạch sẽ hơn, dễ đọc hơn và dễ gỡ lỗi hơn.

## Làm thế nào:
Dưới đây là một đoạn Go mẫu cho thấy một khối code, sau đó là phiên bản đã được tái cấu trúc sử dụng các hàm:

```go
package main

import "fmt"

func main() {
    // Trước: Inline code
    fmt.Println("Đang tính tổng...")
    total := 0
    for i := 1; i <= 10; i++ {
        total += i
    }
    fmt.Println("Tổng cộng là:", total)

    // Sau: Sử dụng một hàm
    fmt.Println("Đang tính tổng bằng một hàm...")
    sum := getSum(1, 10)
    fmt.Println("Tổng cộng là:", sum)
}

// Hàm để tính tổng trong một phạm vi
func getSum(start, end int) int {
    total := 0
    for i := start; i <= end; i++ {
        total += i
    }
    return total
}
```

Kết quả mẫu cho cả code inline và dựa trên hàm sẽ giống nhau:

```
Đang tính tổng...
Tổng cộng là: 55
Đang tính tổng bằng một hàm...
Tổng cộng là: 55
```

## Sâu hơn
Trước khi khái niệm về hàm xuất hiện, lập trình chủ yếu là quy trình, với code chạy từ trên xuống dưới. Khi các chương trình phát triển, cách tiếp cận này đã dẫn đến hiệu suất không hiệu quả và sự lặp lại code.

Các ngôn ngữ giới thiệu hàm như một cơ chế trừu tượng. Trong Go, hàm đóng gói các khối code với một nhiệm vụ cụ thể, khuyến khích nguyên tắc DRY (Don't Repeat Yourself - Đừng Lặp Lại Chính Mình). Chúng chấp nhận các tham số và có thể trả về kết quả.

Mẹo hữu ích:
- Đặt tên cho hàm một cách rõ ràng; một tên tốt giải thích những gì một hàm làm.
- Giữ chúng ngắn gọn; nếu một hàm làm quá nhiều việc, hãy chia nó ra.
- Hàm có thể trả về nhiều giá trị, tận dụng điều đó cho việc xử lý lỗi.
- Hàm bậc cao (hàm nhận hoặc trả về hàm khác) là công cụ mạnh mẽ trong Go.

Các lựa chọn thay thế cho hàm bao gồm code inline (lộn xộn cho các nhiệm vụ phức tạp) và các phương thức đối tượng (một phần của lập trình hướng đối tượng có sẵn trong Go thông qua cấu trúc (structs)).

## Xem thêm
- [Go by Example: Functions](https://gobyexample.com/functions)
- [Effective Go: Function](https://golang.org/doc/effective_go#functions)
