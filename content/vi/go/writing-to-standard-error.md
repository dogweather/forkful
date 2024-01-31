---
title:                "Ghi vào lỗi chuẩn"
date:                  2024-01-28T22:13:21.392593-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi vào lỗi chuẩn"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Việc viết vào lỗi chuẩn (stderr) là cách chương trình của bạn báo cáo lỗi và cảnh báo. Các lập trình viên làm điều này để tách đầu ra thông thường (stdout) khỏi các thông điệp lỗi, làm cho việc xử lý và theo dõi sự cố dễ dàng hơn.

## Làm thế nào:

Trong Go, bạn viết vào lỗi chuẩn sử dụng mô tả tệp `os.Stderr` từ gói `os`. Dưới đây là cách làm:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	message := "Lỗi: có gì đó không đúng!"
	_, err := fmt.Fprintln(os.Stderr, message)

	if err != nil {
		panic(err)
	}
}
```

Ví dụ đầu ra tới stderr có thể trông như thế này:

```
Lỗi: có gì đó không đúng!
```

## Sâu hơn

Theo lịch sử, các hệ điều hành giống Unix cung cấp ba luồng chuẩn: stdin, stdout, và stderr. Go kế thừa khái niệm này. Các lựa chọn khác bao gồm các gói đăng nhập như `log` hoặc `zap`, cung cấp nhiều kiểm soát hơn về định dạng và điểm đến của đầu ra. Khi viết trực tiếp vào stderr, Go sử dụng `os.Stderr`, thực thi `io.Writer`, làm cho nó phù hợp với cách tiếp cận chung của Go đối với I/O bằng cách cung cấp một giao diện được định nghĩa rõ ràng.

## Xem thêm

- Blog Go về xử lý lỗi: https://blog.golang.org/error-handling-and-go
- Tài liệu gói `log`: https://golang.org/pkg/log/
- Bộ ghi `zap`: https://godoc.org/go.uber.org/zap
