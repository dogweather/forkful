---
title:                "Tìm kiếm và thay thế văn bản"
date:                  2024-01-28T22:07:22.271190-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm kiếm và thay thế văn bản"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/searching-and-replacing-text.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc tìm kiếm và thay thế văn bản liên quan đến việc tìm các chuỗi ký tự cụ thể trong một chuỗi và thay thế chúng bằng các ký tự khác. Các lập trình viên làm điều này cho mọi thứ từ việc sửa lỗi chính tả trong các tập dữ liệu lớn đến tự động hóa việc tái cấu trúc code qua nhiều tệp.

## Làm thế nào:

Thư viện tiêu chuẩn `strings` của Go có những gì bạn cần. Dưới đây là cách sử dụng `strings.Replace`:

```go
package main

import (
	"fmt"
	"strings"
)

func main() {
	replacedString := strings.Replace("Hello, Go!", "Go", "Thế giới", -1)
	fmt.Println(replacedString) // Kết quả: Hello, Thế giới!
}
```

`-1` có nghĩa là thay thế tất cả các trường hợp. Để chỉ thay thế trường hợp đầu tiên, hãy sử dụng `1`.

Nếu bạn muốn thực hiện các thay thế phức tạp hơn liên quan đến mẫu, bạn sẽ có khả năng sử dụng `regexp`:

```go
package main

import (
	"fmt"
	"regexp"
)

func main() {
	regex := regexp.MustCompile(`(Go)`)
	replacedString := regex.ReplaceAllString("Hello, Go! Go is great.", "Gopher")
	fmt.Println(replacedString) // Kết quả: Hello, Gopher! Gopher is great.
}
```

Regex là mạnh mẽ, nhưng đừng lạm dụng nó. Đối với những thứ đơn giản, hãy gắn bó với `strings`.

## Đào Sâu

Go không phải là ngôn ngữ đầu tiên thực hiện việc thay thế văn bản, nhưng thư viện tiêu chuẩn của nó lại thân thiện với người dùng. Các công cụ Unix như `sed` đã xử lý việc tìm và thay thế từ lâu trước đó, sử dụng biểu thức chính quy. Gói `regexp` của Go mang lại sức mạnh đó một cách lập trình.

So với các ngôn ngữ khác, Go đánh đổi một chút về tốc độ thô cho sự an toàn và khả năng đọc. Các công cụ và ngôn ngữ khác có thể nhanh hơn trong xử lý văn bản (như Perl), nhưng sự cân bằng của Go giữa sự dễ sử dụng và hiệu suất là một điểm mạnh.

Khi bạn đang thực hiện việc tìm kiếm và thay thế trong Go, nhớ rằng:
- `strings` cho những thứ đơn giản.
- `regexp` cho mẫu.
- Tham số cuối cùng trong `strings.Replace` xác định số lượng thay thế.

## Xem thêm

- Go by Example: Các Hàm Chuỗi - https://gobyexample.com/strings
- Go by Example: Biểu Thức Chính Quy - https://gobyexample.com/regular-expressions
- Gói Go strings - https://pkg.go.dev/strings
- Gói Go regexp - https://pkg.go.dev/regexp
