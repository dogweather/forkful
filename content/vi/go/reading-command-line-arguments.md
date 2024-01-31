---
title:                "Đọc các đối số dòng lệnh"
date:                  2024-01-28T22:05:42.810861-07:00
model:                 gpt-4-0125-preview
simple_title:         "Đọc các đối số dòng lệnh"

category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/reading-command-line-arguments.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc đọc các đối số dòng lệnh cho phép chương trình của bạn nhận đầu vào khi nó được chạy từ terminal, điều này có thể hướng dẫn hành vi của nó mà không cần phải cố định giá trị. Các lập trình viên sử dụng nó để tùy chỉnh việc thực thi phần mềm, xử lý sở thích của người dùng, và phản hồi với các chế độ hoạt động khác nhau.

## Làm thế nào:

Go làm cho việc lấy những đối số dòng lệnh trở nên khá dễ dàng bằng cách sử dụng gói `os`. Đây là cách bạn thực hiện:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	args := os.Args[1:] // os.Args[0] là đường dẫn đến chính chương trình
	for i, arg := range args {
		fmt.Printf("Đối số %d: %s\n", i+1, arg)
	}
}
```

Chạy chương trình của bạn như thế này:

```
$ go run yourprogram.go these are command line args
```

Và bạn sẽ nhận được:

```
Đối số 1: these
Đối số 2: are
Đối số 3: command line
Đối số 4: args
```

Đó là tất cả. Bạn giờ đây có quyền lực để ảnh hưởng đến hành vi của chương trình từ terminal.

## Sâu hơn

Trước khi GUI ra đời, các đối số dòng lệnh là tiêu chuẩn cho việc báo cho các chương trình biết phải làm gì. Chúng bắt nguồn từ các quy ước của UNIX, mà Go kế thừa một phần do mối quan hệ tương thích với môi trường POSIX.

Các phương pháp thay thế cho phân tích đối số trong Go bao gồm việc sử dụng các gói phức tạp hơn như `flag` cho các cờ (ví dụ, `--name=value`) hoặc các thư viện bên thứ ba như `cobra` hoặc `urfave/cli` để xây dựng các ứng dụng CLI phức tạp.

Mảng `os.Args` chứa tất cả các đối số, với `os.Args[0]` là chính chương trình. Sự đơn giản của nó hoàn hảo cho các nhiệm vụ đơn giản, nhưng hãy cảnh giác với các trường hợp cần các lệnh có cấu trúc hoặc cờ.

## Xem Thêm

- Gói `flag` cho một lựa chọn phân tích mạnh mẽ hơn: [https://pkg.go.dev/flag](https://pkg.go.dev/flag)
- Cobra để xây dựng các ứng dụng dòng lệnh mạnh mẽ: [https://github.com/spf13/cobra](https://github.com/spf13/cobra)
- `urfave/cli` cho một gói đơn giản, nhanh chóng và vui vẻ để xây dựng CLIs trong Go: [https://github.com/urfave/cli](https://github.com/urfave/cli)
