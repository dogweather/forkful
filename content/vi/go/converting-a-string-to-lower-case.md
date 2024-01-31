---
title:                "Chuyển đổi chuỗi thành chữ thường"
date:                  2024-01-28T21:57:50.179795-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi chuỗi thành chữ thường"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Lý do & Tại sao?
Việc chuyển một chuỗi sang chữ thường có nghĩa là biến đổi tất cả các ký tự chữ cái trong văn bản thành dạng chữ thường tương đương. Các lập trình viên thực hiện việc này để đạt được sự nhất quán, đặc biệt trong các so sánh không phân biệt chữ hoa chữ thường, chuẩn hóa dữ liệu, và để ngăn chặn các bản ghi trùng lặp chỉ khác biệt về chữ cái.

## Cách thực hiện:
Trong Go, sử dụng `strings.ToLower` để chuyển một chuỗi sang chữ thường. Dưới đây là cách thực hiện:

```go
package main

import (
	"fmt"
	"strings"
)

func main() {
	original := "Hello, World!"
	lower := strings.ToLower(original)
	fmt.Println(lower) // Kết quả: hello, world!
}
```
Chạy mã. Đầu ra là phiên bản của chuỗi đã được chuyển thành chữ thường.

## Sâu hơn
Khái niệm về việc chuyển đổi chữ hoa chữ thường đã tồn tại từ rất lâu kể từ khi có sự xuất hiện của chữ cái chữ hoa và chữ thường. Go xử lý điều này với gói `strings`, cung cấp một cách đơn giản, hiệu quả để biến đổi chuỗi.

Có phương án khác không? Chắc chắn. Bạn có thể lặp qua từng ký tự để kiểm tra trường hợp của chúng một cách thủ công, nhưng tại sao lại phải tái phát minh bánh xe?

Về mặt triển khai, `ToLower` phức tạp hơn nhiều so với vẻ bề ngoài của nó. Nó nhận biết về Unicode và xử lý đúng cách các ký tự ngoài bộ ASCII cơ bản. Điều này có nghĩa là nó sẽ biến các ký tự từ tiếng Hy Lạp, Cyrillic, v.v., không chỉ là bảng chữ cái tiếng Anh, thành chữ thường.

## Xem thêm
Để biết thêm, hãy xem:
- Tài liệu gói `strings` của Go: https://pkg.go.dev/strings
- Tiêu chuẩn Unicode: https://www.unicode.org/standard/standard.html
- Go ví dụ: Chuỗi - https://gobyexample.com/strings
