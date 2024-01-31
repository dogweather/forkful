---
title:                "Sử dụng biểu thức chính quy"
date:                  2024-01-28T22:10:08.160082-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng biểu thức chính quy"

category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/using-regular-expressions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Biểu thức chính quy (regex) là các mẫu được sử dụng để khớp các kết hợp ký tự trong chuỗi. Lập trình viên sử dụng chúng cho việc tìm kiếm, xác thực, và thao tác văn bản, biến chúng thành công cụ đa năng cho các thao tác chuỗi.

## Cách thực hiện:
```Go
package main

import (
    "fmt"
    "regexp"
)

func main() {
    // Ví dụ: Tìm email trong một chuỗi
    text := "Reach out at contact@example.com or support@random.org"
    emailRegex := regexp.MustCompile(`[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}`)

    // FindString trả về kết quả đầu tiên tìm thấy
    fmt.Printf("Email đầu tiên: %s\n", emailRegex.FindString(text)) 
    // Kết quả: Email đầu tiên: contact@example.com

    // FindAllString trả về tất cả kết quả tìm thấy
    emails := emailRegex.FindAllString(text, -1)
    fmt.Printf("Tất cả email: %v\n", emails) 
    // Kết quả: Tất cả email: [contact@example.com support@random.org]

    // Thay thế văn bản
    sanitizedText := emailRegex.ReplaceAllString(text, "[redacted]")
    fmt.Println(sanitizedText) 
    // Kết quả: Reach out at [redacted] or [redacted]
}
```

## Kỹ thuật sâu
Regex có nguồn gốc từ Unix vào những năm 1950, được phổ biến thông qua các công cụ như `grep`. Sau đó, Perl đã làm cho chúng trở nên phổ biến. Các phương án thay thế bao gồm sử dụng các hàm chuỗi hoặc bộ phân tích cú pháp cho dữ liệu đơn giản và có cấu trúc, tương ứng. Về mặt triển khai, gói `regexp` của Go dựa trên NFA (non-deterministic finite automaton), xử lý regex một cách hiệu quả mà không gặp các vấn đề về lùi bước tìm thấy trong một số engine khác.

## Xem thêm
- Tài liệu gói `regexp` của Go: [pkg.go.dev/regexp](https://pkg.go.dev/regexp)
- Trình kiểm tra và gỡ lỗi regex trực tuyến: [regex101.com](https://regex101.com/)
- Hướng dẫn regex của Mạng Lưới Nhà Phát Triển Mozilla: [developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
