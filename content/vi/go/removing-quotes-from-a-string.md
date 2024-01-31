---
title:                "Loại bỏ dấu ngoặc kép khỏi chuỗi"
date:                  2024-01-28T22:06:46.150662-07:00
model:                 gpt-4-0125-preview
simple_title:         "Loại bỏ dấu ngoặc kép khỏi chuỗi"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/removing-quotes-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại Sao?

Loại bỏ dấu ngoặc kép khỏi một chuỗi có nghĩa là loại bỏ những ký tự dấu ngoặc kép hoặc ngoặc đơn gây phiền phức bao quanh văn bản thực của bạn. Chúng ta làm điều này để làm sạch dữ liệu, ngăn chặn lỗi phân tích cú pháp hoặc chuẩn bị văn bản cho quá trình xử lý tiếp theo mà không có thêm phần rườm rà của dấu ngoặc kép.

## Làm thế nào:

Đây là cách đơn giản để loại bỏ những dấu ngoặc đó trong Go:

```go
package main

import (
	"fmt"
	"strings"
)

func removeQuotes(s string) string {
	return strings.Trim(s, "'\"")
}

func main() {
	quotedString := "\"Xin chào, Thế giới!\""
	fmt.Println("Bản gốc:", quotedString)

	unquotedString := removeQuotes(quotedString)
	fmt.Println("Không dấu ngoặc:", unquotedString)
}
```

Đầu ra sẽ trông như thế này, tất cả các dấu ngoặc đều biến mất:

```
Bản gốc: "Xin chào, Thế giới!"
Không dấu ngoặc: Xin chào, Thế giới!
```

## Sâu hơn

Ngày xưa, khi các định dạng và sự trao đổi dữ liệu chưa được chuẩn hóa, dấu ngoặc kép trong chuỗi có thể gây hỗn loạn. Chúng vẫn có thể gây ra vấn đề, đặc biệt là trong JSON hoặc khi chèn chuỗi vào cơ sở dữ liệu. Gói `strings` trong Go được tải với hàm `Trim`, loại bỏ không chỉ khoảng trắng mà còn bất kỳ ký tự nào bạn không thích.

Tại sao không dùng Regex? Vâng, `Trim` nhanh hơn cho những công việc đơn giản, nhưng nếu chuỗi của bạn đang chơi trốn tìm với các dấu ngoặc ở những nơi lạ lùng, regex có thể là vũ khí hạng nặng của bạn:

```go
import "regexp"

func removeQuotesWithRegex(s string) string {
	re := regexp.MustCompile(`^["']|["']$`)
	return re.ReplaceAllString(s, "")
}
```

Giống như việc chọn giữa kéo và cưa máy; chọn công cụ phù hợp cho công việc.

## Xem Thêm

Để biết thêm về gói `strings` và các công cụ mạnh mẽ của nó:
- [Gói strings](https://pkg.go.dev/strings)

Để sử dụng sức mạnh của biểu thức chính quy trong Go:
- [Gói regexp](https://pkg.go.dev/regexp)

Muốn tìm hiểu sâu hơn về triết lý của việc cắt chuỗi?
- [Phương pháp Trim](https://blog.golang.org/strings)
