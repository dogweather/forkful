---
title:                "Nối chuỗi ký tự"
date:                  2024-01-28T21:57:18.842888-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nối chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/concatenating-strings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?
Nối chuỗi là quá trình ghép hai hoặc nhiều chuỗi lại với nhau từ đầu đến cuối. Lập trình viên làm điều này để tạo ra các chuỗi mới từ những chuỗi hiện có, cho dù là để xây dựng thông điệp, tạo nội dung động, hay chỉ đơn giản là định hình văn bản cho phù hợp với tình huống.

## Làm thế nào:
Đây là cách đơn giản nhất để làm cho các chuỗi trong Go gắn kết với nhau.

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	// Sử dụng toán tử +
	hello := "Hello"
	world := "World"
	result := hello + ", " + world + "!"

	fmt.Println(result) // Output: Hello, World!
	
	// Sử dụng fmt.Sprintf
	message := fmt.Sprintf("%s, %s!", hello, world)
	
	fmt.Println(message) // Output: Hello, World!
	
	// Sử dụng strings.Builder
	var sb strings.Builder
	sb.WriteString(hello)
	sb.WriteString(", ")
	sb.WriteString(world)
	sb.WriteString("!")
	
	fmt.Println(sb.String()) // Output: Hello, World!
	
	// Sử dụng strings.Join cho slices
	parts := []string{hello, world}
	combined := strings.Join(parts, ", ")

	fmt.Println(combined + "!") // Output: Hello, World!
}
```

## Tìm hiểu sâu hơn
Việc nối chuỗi khá là đơn giản nhưng rất quan trọng trong lập trình. Lịch sử cho thấy, nhu cầu về nối chuỗi đã tồn tại từ những ngày đầu của lập trình. Khi các ngôn ngữ phát triển, phương pháp nối chuỗi cũng vậy. Trong Go, sử dụng toán tử `+` là phương pháp trực tiếp nhất, nhưng không phải lúc nào cũng hiệu quả nhất, đặc biệt là trong vòng lặp.

Các phương án khác như `fmt.Sprintf` và `strings.Builder` mang lại nhiều quyền kiểm soát và hiệu suất hơn. `fmt.Sprintf` linh hoạt cho định dạng, nhưng `strings.Builder` là lựa chọn hàng đầu về hiệu suất, đặc biệt khi xây dựng chuỗi dài từ nhiều mảnh. Trước khi `strings.Builder` được thêm vào (trong Go 1.10), việc nối chuỗi trong vòng lặp thường dẫn đến các vấn đề về hiệu suất do phân bổ bộ nhớ và thu gom rác.

Chuỗi trong Go là bất biến, và khi bạn sử dụng toán tử `+`, một chuỗi mới được tạo ra mỗi lần. Điều này có thể dẫn đến sự không hiệu quả về bộ nhớ. Ưu điểm của việc sử dụng `strings.Builder` là nó ghi vào một bộ đệm có thể mở rộng, giảm thiểu việc phân bổ bộ nhớ.

## Xem thêm
- Blog chính thức của Go về chuỗi: https://blog.golang.org/strings
- Tài liệu gói `strings`: https://pkg.go.dev/strings
- Tài liệu gói `fmt`: https://pkg.go.dev/fmt
- Wiki của Go: https://github.com/golang/go
