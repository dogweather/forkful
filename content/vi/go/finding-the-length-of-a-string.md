---
title:                "Tìm chiều dài của một chuỗi ký tự"
date:                  2024-01-28T22:00:45.543708-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm chiều dài của một chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tìm độ dài của một chuỗi có nghĩa là xác định xem nó chứa bao nhiêu ký tự. Lập trình viên thực hiện điều này để xác nhận đầu vào, lặp qua các ký tự, giới hạn đầu ra, và nhiều hơn nữa.

## Cách thực hiện:

Để lấy độ dài của một chuỗi, sử dụng `len()`:

```Go
package main

import "fmt"

func main() {
    exampleStr := "Hello, Gophers!"
    length := len(exampleStr)
    fmt.Println(length)  // Đầu ra: 14
}
```

Đối với các ký tự Unicode hoặc emoji, `utf8.RuneCountInString()` là người bạn của bạn:

```Go
package main

import (
    "fmt"
    "unicode/utf8"
)

func main() {
    exampleStr := "Hello, 世界!"
    length := utf8.RuneCountInString(exampleStr)
    fmt.Println(length)  // Đầu ra: 9
}
```

## Nhìn sâu hơn
Nói một cách đơn giản, Go sử dụng chuỗi được mã hóa UTF-8. Hàm `len()` đã tích hợp sẵn trả về số byte, không phải số lượng ký tự. Điều này nhanh chóng nhưng có thể gây ra bất ngờ với các ký tự đa byte. Để đếm số lượng ký tự chính xác, đặc biệt là trong các ứng dụng toàn cầu, hãy sử dụng `utf8.RuneCountInString()` để xử lý Unicode một cách chính xác. Trong lịch sử, các ngôn ngữ và thư viện khác nhau đã đếm ký tự theo những cách khác nhau, nhưng Unicode đã trở thành tiêu chuẩn, và việc Go hỗ trợ nó là bắt buộc trong hệ sinh thái mã hóa đa dạng ngày nay.

Về các phương án thay thế, các thư viện như `unicode/utf8` cung cấp cách xử lý vững chắc cho runes, đại diện cho các điểm mã Unicode. Trước khi Go chuẩn hóa việc xử lý Unicode, lập trình viên phải thực hiện các giải pháp tùy chỉnh, điều này dễ gặp lỗi và phức tạp.

Trong chi tiết triển khai, chuỗi trong Go là các chuỗi byte bất biến. Khi xử lý chuỗi, lập trình viên nên nhận thức về khả năng giảm hiệu năng khi xử lý các chuỗi rất lớn hoặc khi sử dụng `utf8.RuneCountInString()` một cách quá mức trong mã hiệu năng quan trọng, vì nó phải giải mã từng rune để đếm một cách chính xác.

## Xem thêm
- Blog của Go về Chuỗi: https://blog.golang.org/strings
- Tài liệu gói `unicode/utf8` của Go: https://golang.org/pkg/unicode/utf8/
- Quy định về hàm `len` của Go: https://golang.org/ref/spec#Length_and_capacity
