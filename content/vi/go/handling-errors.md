---
title:                "Xử lý lỗi"
date:                  2024-01-28T22:01:54.919173-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xử lý lỗi"
programming_language: "Go"
category:             "Go"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/handling-errors.md"
changelog:
  - 2024-01-21, dogweather, Reviewed for accuracy
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại Sao?

Xử lý lỗi trong Go là việc bắt và phản hồi một cách nhẹ nhàng trước những sự cố khi chạy chương trình. Chúng ta làm điều này để ngăn chặn sự cố và đảm bảo rằng chương trình của chúng ta hoạt động một cách dự đoán được, ngay cả khi có điều gì đó không như ý muốn.

## Làm thế nào:

Go sử dụng cách xử lý lỗi rõ ràng. Điều đó có nghĩa là bạn sẽ kiểm tra xem một hàm trả về lỗi hay không mỗi khi bạn gọi nó. Không có Ngoại lệ. Dưới đây là cách thực hiện:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	err := doSomething()
	if err != nil {
		fmt.Println("Ối:", err)
		os.Exit(1)
	}
}

func doSomething() error {
	// Giả sử có điều gì đó không ổn
	return fmt.Errorf("có điều gì đó sai sai")
}
```

Chạy cái này, và bạn sẽ nhận được:

```
Ối: có điều gì đó sai sai
```

Nhưng nếu nó thành công thì sao?

```Go
func doSomething() error {
	// Mọi thứ tốt đẹp lần này
	return nil
}
```

Không có đầu ra. Tuyệt, không tin tức nào cũng là tin tốt.

## Sâu Hơn:

Trong Go, việc xử lý lỗi đã là điểm gây tranh cãi. Ngay từ ban đầu, Go đã quyết định không sử dụng ngoại lệ để áp dụng cách tiếp cận rõ ràng hơn, điều này một số nhà phát triển yêu thích vì sự đơn giản và những người khác thấy nó dài dòng. Kiểu `error` tích hợp là một interfàce. Bất kỳ kiểu nào có phương thức `Error() string` đều thoả mãn nó. Điều này gắn liền với tinh thần đơn giản và rõ ràng của Go.

Các phương án khác? Có cặp `panic` và `recover`, nhưng chúng dành cho những trường hợp đặc biệt (đúng như ý định từ ngữ) khi chương trình không thể tiếp tục. Hãy nghĩ về `panic` như là nút thoát mà bạn nhấn khi bạn biết rằng không có lối thoát. Sử dụng nó một cách tiết kiệm.

Về việc xử lý lỗi chính thống, Go 1.13 giới thiệu việc đóng gói lỗi, làm cho việc tìm ra "chuỗi lỗi" dễ dàng hơn với các hàm như `errors.Is()` và `errors.As()`.

## Tham Khảo:

Về mọi thứ liên quan đến xử lý lỗi trong Go:

- Blog của Go về Xử lý Lỗi: [https://blog.golang.org/error-handling-and-go](https://blog.golang.org/error-handling-and-go)
- Go Hiệu quả – Phần xử lý lỗi: [https://golang.org/doc/effective_go#errors](https://golang.org/doc/effective_go#errors)
- Tài liệu Go 1.13 về Đóng gói Lỗi: [https://golang.org/doc/go1.13#error_wrapping](https://golang.org/doc/go1.13#error_wrapping)
- Bài viết của Dave Cheney về các chiến lược xử lý lỗi: [https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully](https://dave.cheney.net/2016/04/27/dont-just-check-errors-handle-them-gracefully)
