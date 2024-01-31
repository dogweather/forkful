---
title:                "Viết hoa một chuỗi"
date:                  2024-01-28T21:55:50.841366-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết hoa một chuỗi"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Việc viết hoa một chuỗi chuyển đổi chữ cái đầu tiên của một chuỗi đã cho thành chữ in hoa. Các lập trình viên làm điều này để định dạng đầu ra, tuân theo các quy tắc ngữ pháp, hoặc làm cho văn bản dễ đọc hơn.

## Cách thực hiện:
Trong Go, chuỗi là bất biến, vì vậy bạn cần tạo một phiên bản viết hoa mới của chuỗi. Chúng ta sử dụng gói `strings` và hàm `Title` của nó hoặc điều chỉnh trực tiếp các rune của chuỗi:

```Go
package main

import (
	"fmt"
	"strings"
	"unicode"
)

func main() {
	// Ví dụ 1: Sử dụng strings.Title để viết hoa mỗi từ
	fmt.Println(strings.Title("hello world!")) // Xuất ra: Hello World!

	// Ví dụ 2: Viết hoa chỉ ký tự đầu tiên
	input := "hello again!"
	if len(input) > 0 {
		fmt.Println(strings.ToUpper(string(input[0])) + input[1:]) // Xuất ra: Hello again!
	}

	// Ví dụ 3: Viết hoa chính xác hơn, xử lý các ký tự đa byte
	capitalizeFirst := func(s string) string {
		for i, v := range s {
			return string(unicode.ToUpper(v)) + s[i+utf8.RuneLen(v):]
		}
		return ""
	}

	fmt.Println(capitalizeFirst("привет мир!")) // Xuất ra: Привет мир!
}
```

## Thảo sâu hơn
Việc viết hoa chuỗi không phải là quá trình phức tạp, nhưng có rất nhiều điều diễn ra bên dưới. Trước khi hàm `strings.Title` tồn tại, bạn cần phải điều chỉnh chuỗi ở cấp độ rune để viết hoa đúng cách.

Trong các ngôn ngữ lập trình cũ hơn, việc xử lý các ký tự không phải ASCII khi viết hoa là khó khăn do thiếu hỗ trợ Unicode đúng cách. Go giúp điều này dễ dàng hơn với hỗ trợ tích hợp cho mã hóa UTF-8 trong các gói tiêu chuẩn `unicode` và `utf8`.

Khi tự viết hoa chuỗi trong Go, hãy nhớ xử lý các ký tự đa byte. Đó là lý do tại sao chúng tôi lặp qua chuỗi bằng cách sử dụng `range` trong ví dụ chắc chắn, là lặp qua runes thay vì bytes.

Có các phương pháp thay thế cho các phương thức Go tích hợp, như sử dụng thư viện bên thứ ba cho các nhu cầu điều chỉnh văn bản phức tạp hơn. Tuy nhiên, đối với việc viết hoa đơn giản, thư viện tiêu chuẩn của Go thường là đủ.

## Xem thêm
- Gói chuỗi Go: [https://golang.org/pkg/strings/](https://golang.org/pkg/strings/)
- Gói Unicode Go: [https://golang.org/pkg/unicode/](https://golang.org/pkg/unicode/)
- Gói utf8 Go: [https://golang.org/pkg/unicode/utf8/](https://golang.org/pkg/unicode/utf8/)
- Một bài viết hay về chuỗi và runes trong Go: [https://blog.golang.org/strings](https://blog.golang.org/strings)
