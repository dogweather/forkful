---
title:                "Trích xuất chuỗi con"
date:                  2024-01-28T21:59:57.591829-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trích xuất chuỗi con"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái Gì và Tại Sao?
Trích xuất các chuỗi con có nghĩa là cắt xén các phần của một chuỗi. Các lập trình viên thực hiện việc này để cô lập, phân tích, hoặc thao tác với các bit dữ liệu cụ thể bên trong một chuỗi lớn hơn.

## Làm Như Thế Nào:
Go làm cho việc này trở nên dễ dàng với thư viện chuẩn và việc cắt chuỗi. Dưới đây là thông tin chi tiết:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	phrase := "The quick brown fox jumps over the lazy dog"
	
	// Sử dụng cắt chuỗi
	part := phrase[4:9]
	fmt.Println(part) // Kết quả: quick
	
	// Sử dụng gói strings
	start := strings.Index(phrase, "brown")
	end := start + len("brown")
	substring := phrase[start:end]
	fmt.Println(substring) // Kết quả: brown
}
```

## Sâu hơn
Một bài học lịch sử nhanh chóng: Go ra mắt vào năm 2009 như một dự án mã nguồn mở để làm cho việc lập trình thêm vui vẻ và hiệu quả. Nó giữ việc thao tác chuỗi trở nên đơn giản—không cần biểu thức chính quy cho các nhiệm vụ đơn giản. Các ngôn ngữ khác như Python có các cơ chế cắt chuỗi tương tự.

Chắc chắn, có những phương án thay thế như gói `regexp` và `bytes` cho công việc nặng nhọc. Tuy nhiên, hàm `Index` cơ bản và việc cắt chuỗi đã đáp ứng hầu hết nhu cầu mà không gặp rắc rối. Bên dưới cùng, chuỗi trong Go chỉ là các lát byte. Vì vậy, khi bạn cắt một chuỗi, bạn thực sự đang tạo một tiêu đề lát mới trỏ đến mảng cơ bản của chuỗi gốc. Điều này làm cho việc trích xuất chuỗi con trong Go nhanh chóng và hiệu quả về bộ nhớ.

## Xem Thêm
- Gói `strings` của Go: https://pkg.go.dev/strings
- Go Slices: sử dụng và nội bộ: https://blog.golang.org/slices
- Go by Example: Strings: https://gobyexample.com/strings
