---
title:                "Kiểm tra xem thư mục có tồn tại không"
date:                  2024-01-28T21:56:10.194727-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kiểm tra xem thư mục có tồn tại không"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/checking-if-a-directory-exists.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Kiểm tra xem một thư mục có tồn tại hay không có nghĩa là xác nhận liệu một thư mục cụ thể có mặt trên hệ thống tệp hay không. Lập trình viên làm điều này để tránh lỗi, như cố gắng đọc từ hoặc viết vào một thư mục không tồn tại.

## Làm thế nào:
Thư viện chuẩn của Go làm cho việc này trở nên dễ dàng. Sử dụng `os.Stat` và kiểm tra lỗi với `os.IsNotExist`:

```go
package main

import (
	"fmt"
	"os"
)

func main() {
	dir := "/path/to/your/directory"
	if _, err := os.Stat(dir); os.IsNotExist(err) {
		fmt.Printf("Oops: %v\n", err)
	} else {
		fmt.Println("Yep, nó tồn tại!")
	}
}
```

Kết quả mẫu nếu thư mục không tồn tại:

```
Oops: stat /path/to/your/directory: no such file or directory
```

Và nếu nó tồn tại:

```
Yep, nó tồn tại!
```

## Sâu hơn
Việc kiểm tra “sự tồn tại” này đã là một phần của Go từ những ngày đầu, là một phần của gói `os` mạnh mẽ. Có một cách khác: `ioutil.ReadDir` đọc thư mục và trả về một lỗi nếu nó không tồn tại. Nhưng tại sao phải lo lắng? Việc này kém hiệu quả hơn chỉ để kiểm tra sự tồn tại.

Phía dưới, `os.Stat` thực hiện một cuộc gọi hệ thống để lấy thông tin về tệp hoặc thư mục. Không cần phải thực hiện một cuộc gọi cho mỗi tệp khi chỉ cần một cuộc gọi là đủ.

Trước đây, lập trình viên thường "chạm" vào một tệp trong thư mục, nhưng đó là I/O không cần thiết. Chúng ta muốn mã hiệu quả và tinh tế. Go thực hiện điều này một cách đơn giản.

## Xem thêm
- Tài liệu gói `os` của Go: https://pkg.go.dev/os#Stat
- Thao tác với hệ thống tệp trong Go: https://golang.org/pkg/io/ioutil/#ReadDir
- Thêm về xử lý lỗi trong Go: https://blog.golang.org/error-handling-and-go
