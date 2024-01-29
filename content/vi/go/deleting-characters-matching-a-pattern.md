---
title:                "Xóa các ký tự phù hợp với một mẫu"
date:                  2024-01-28T21:59:00.337249-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xóa các ký tự phù hợp với một mẫu"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Lý do & Tại sao?

Xóa các ký tự khớp với một mẫu là về việc rút ra những phần cụ thể từ một chuỗi - giống như nhổ cỏ từ vườn văn bản của bạn. Lập trình viên làm điều này để dọn dẹp, định dạng, hoặc phân tích dữ liệu, đảm bảo văn bản được tinh khiết và hữu ích.

## Cách thực hiện:

Trong Go, chúng ta sử dụng các gói `strings` và `regexp`. Dưới đây là những điều cần biết kèm theo mã code:

```go
package main

import (
	"fmt"
	"regexp"
	"strings"
)

func main() {
	// Sử dụng gói strings để xóa một tập hợp ký tự
	str1 := "Hello, 123 World!"
	cleanStr1 := strings.Map(func(r rune) rune {
		if r >= '0' && r <= '9' {
			return -1 // Delete char
		}
		return r // Keep char
	}, str1)

	fmt.Println(cleanStr1) // Output: Hello,  World!

	// Sử dụng gói regexp để xóa các ký tự khớp với một mẫu
	str2 := "Go 1.18 is the current version!"
	re := regexp.MustCompile(`[0-9]+`)
	cleanStr2 := re.ReplaceAllString(str2, "")

	fmt.Println(cleanStr2) // Output: Go . is the current version!
}
```

## Sâu hơn

Trở lại những ngày xưa cũ, khi ngôn ngữ lập trình giống như những phép thuật bí ấn, khớp mẫu là một kỹ năng được ao ước. Biểu thức chính quy (regex) là công cụ đa năng cho công việc này. Go, tuy nhiên, đã làm cho nó trở nên dễ dàng và hiệu quả, tích hợp sức mạnh này với gói `regexp`.

Vậy, tại sao bạn không chỉ sử dụng `strings.Replace` hoặc `strings.ReplaceAll`? À, những cái đấy tốt cho việc thay thế đơn giản, tĩnh. Nhưng khi các mẫu của bạn hoang dại như dây leo trong rừng, regex là nơi bạn chuyển hướng đến.

Bên dưới lớp vỏ, `regexp` biên dịch một mẫu thành một máy trạng thái. Mỗi ký tự được kiểm tra chống lại máy này và các phần khớp được loại bỏ. Điều này có nghĩa là công việc nặng nhọc là biên dịch lần đầu, nhưng nhanh chóng vô cùng sau đó.

Phương pháp thay thế? Bạn có `bytes.Buffer` để xây dựng các chuỗi không theo mẫu và `strings.Builder` trong các phiên bản mới hơn khi bạn dị ứng với việc phân bổ không cần thiết.

## Xem thêm

Những nơi bạn có thể tìm hiểu thêm:
- Go by Example: Regular Expressions - https://gobyexample.com/regular-expressions
- Go Doc: Package strings - https://pkg.go.dev/strings
- Go Doc: Package regexp - https://pkg.go.dev/regexp
- Regular Expression Playground - https://regex101.com/ (Không cụ thể cho Go, nhưng rất tiện lợi)
