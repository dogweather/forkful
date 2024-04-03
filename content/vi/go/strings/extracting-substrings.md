---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:00.585671-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Go, ki\u1EC3u `string` l\xE0 m\u1ED9t\
  \ m\u1EA3ng byte ch\u1EC9 \u0111\u1ECDc. \u0110\u1EC3 tr\xEDch xu\u1EA5t c\xE1c\
  \ chu\u1ED7i con, ng\u01B0\u1EDDi ta ch\u1EE7 y\u1EBFu s\u1EED d\u1EE5ng c\xFA ph\xE1\
  p `slice`, c\xF9ng v\u1EDBi h\xE0m `len()`\u2026"
lastmod: '2024-03-13T22:44:35.966421-06:00'
model: gpt-4-0125-preview
summary: "Trong Go, ki\u1EC3u `string` l\xE0 m\u1ED9t m\u1EA3ng byte ch\u1EC9 \u0111\
  \u1ECDc."
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
weight: 6
---

## Làm thế nào:
Trong Go, kiểu `string` là một mảng byte chỉ đọc. Để trích xuất các chuỗi con, người ta chủ yếu sử dụng cú pháp `slice`, cùng với hàm `len()` được tích hợp sẵn để kiểm tra độ dài và gói `strings` cho các thao tác phức tạp hơn. Dưới đây là cách bạn có thể thực hiện:

### Cắt Cơ Bản
```go
package main

import (
    "fmt"
)

func main() {
    str := "Hello, World!"
    // Trích xuất "World"
    subStr := str[7:12]
    
    fmt.Println(subStr) // Đầu ra: World
}
```

### Sử dụng Gói `strings`
Đối với việc trích xuất chuỗi con tiên tiến hơn, như trích xuất chuỗi sau hoặc trước một chuỗi con cụ thể, bạn có thể sử dụng gói `strings`.

```go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "name=John Doe"
    // Trích xuất chuỗi con sau "="
    subStr := strings.SplitN(str, "=", 2)[1]
    
    fmt.Println(subStr) // Đầu ra: John Doe
}
```

Cần lưu ý rằng chuỗi Go được mã hóa UTF-8 và một mảng byte trực tiếp không phải lúc nào cũng tạo ra chuỗi hợp lệ nếu chúng bao gồm các ký tự nhiều byte. Để hỗ trợ Unicode, bạn nên xem xét sử dụng `range` hoặc gói `utf8`.

### Xử Lý Ký Tự Unicode
```go
package main

import (
    "fmt"
    "unicode/utf8"
)

func main() {
    str := "Hello, 世界"
    // Tìm chuỗi con xem xét ký tự Unicode
    runeStr := []rune(str)
    subStr := string(runeStr[7:])
    
    fmt.Println(subStr) // Đầu ra: 世界
}
```

## Sâu hơn
Việc trích xuất chuỗi con trong Go khá đơn giản, nhờ cú pháp slice và thư viện tiêu chuẩn đầy đủ. Trong lịch sử, các ngôn ngữ lập trình trước đây cung cấp các hàm hoặc phương pháp trực tiếp hơn để xử lý việc thao tác văn bản như vậy. Tuy nhiên, cách tiếp cận của Go nhấn mạnh vào an toàn và hiệu quả, đặc biệt với chuỗi không thể thay đổi và việc xử lý rõ ràng các ký tự Unicode thông qua runes.

Mặc dù việc cắt trực tiếp thuận tiện có lợi từ hiệu quả hiệu suất, nó thừa hưởng sự phức tạp của việc xử lý trực tiếp các ký tự UTF-8. Sự giới thiệu của kiểu `rune` cho phép các chương trình Go xử lý một cách an toàn văn bản Unicode, tạo nên một lựa chọn mạnh mẽ cho các ứng dụng quốc tế.

Hơn nữa, các lập trình viên đến từ các ngôn ngữ khác có thể nhớ các hàm hoặc phương pháp thao tác chuỗi cấp cao tích hợp sẵn. Tuy nhiên, các gói `strings` và `bytes` trong thư viện tiêu chuẩn của Go cung cấp một bộ chức năng phong phú mạnh mẽ cho việc xử lý chuỗi, bao gồm trích xuất chuỗi con, mặc dù cần một chút khung sườn cứng nhắc hơn.

Bản chất, các lựa chọn thiết kế của Go xung quanh thao tác chuỗi phản ánh mục tiêu của nó về sự đơn giản, hiệu suất và an toàn khi xử lý dữ liệu văn bản hiện đại, quốc tế hóa. Mặc dù có thể cần một chút điều chỉnh, Go cung cấp các công cụ hiệu quả và hiệu suất cao cho việc xử lý trích xuất chuỗi con và hơn thế nữa.
