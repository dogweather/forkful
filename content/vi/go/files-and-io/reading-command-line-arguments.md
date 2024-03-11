---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:06:41.846770-07:00
description: "Vi\u1EC7c \u0111\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7\
  nh trong Go bao g\u1ED3m vi\u1EC7c tr\xEDch xu\u1EA5t c\xE1c \u0111\u1ED1i s\u1ED1\
  \ \u0111\u01B0\u1EE3c cung c\u1EA5p cho m\u1ED9t ch\u01B0\u01A1ng tr\xECnh trong\
  \ qu\xE1 tr\xECnh \u0111\u01B0\u1EE3c g\u1ECDi t\u1EEB terminal ho\u1EB7c d\u1EA5\
  u\u2026"
lastmod: '2024-03-11T00:14:09.227945-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c \u0111\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh trong\
  \ Go bao g\u1ED3m vi\u1EC7c tr\xEDch xu\u1EA5t c\xE1c \u0111\u1ED1i s\u1ED1 \u0111\
  \u01B0\u1EE3c cung c\u1EA5p cho m\u1ED9t ch\u01B0\u01A1ng tr\xECnh trong qu\xE1\
  \ tr\xECnh \u0111\u01B0\u1EE3c g\u1ECDi t\u1EEB terminal ho\u1EB7c d\u1EA5u\u2026"
title: "\u0110\u1ECDc c\xE1c tham s\u1ED1 d\xF2ng l\u1EC7nh"
---

{{< edit_this_page >}}

## Gì và Tại sao?

Việc đọc các đối số dòng lệnh trong Go bao gồm việc trích xuất các đối số được cung cấp cho một chương trình trong quá trình được gọi từ terminal hoặc dấu nhắc lệnh. Lập trình viên thực hiện điều này để tùy chỉnh việc thực thi chương trình mà không cần thay đổi mã, làm cho ứng dụng linh hoạt hơn và do người dùng điều khiển.

## Làm thế nào:

Go cung cấp quyền truy cập trực tiếp vào các đối số dòng lệnh thông qua gói `os`, cụ thể sử dụng `os.Args`, một mảng các chuỗi. Dưới đây là một ví dụ đơn giản để bắt đầu:

```go
package main

import (
    "fmt"
    "os"
)

func main() {
    // os.Args cung cấp quyền truy cập vào các đối số dòng lệnh thô
    fmt.Println("Các đối số dòng lệnh:", os.Args)

    if len(os.Args) > 1 {
        // Duyệt qua các đối số, bỏ qua đối số đầu tiên (tên chương trình)
        for i, arg := range os.Args[1:] {
            fmt.Printf("Đối số %d: %s\n", i+1, arg)
        }
    } else {
        fmt.Println("Không có đối số dòng lệnh nào được cung cấp.")
    }
}
```

Kết quả mẫu khi chạy với `go run yourprogram.go arg1 arg2` có thể trông như sau:

```
Các đối số dòng lệnh: [/tmp/go-build123456789/b001/exe/yourprogram arg1 arg2]
Đối số 1: arg1
Đối số 2: arg2
```

Điều này in ra tất cả các đối số bao gồm tên chương trình (thường ở chỉ số 0), sau đó lặp qua từng đối số được cung cấp, in chúng ra. Để giải quyết việc phân tích đối số một cách có kiểm soát hơn, bạn có thể cân nhắc sử dụng gói `flag` để phân tích các tùy chọn dòng lệnh.

## Sâu hơn

Trong lịch sử, việc truy cập vào các đối số dòng lệnh là một thực hành cũ như lập trình C, nơi `argc` và `argv[]` phục vụ một mục đích tương tự. Trong Go, `os.Args` làm cho việc này trở nên đơn giản nhưng cố ý sơ khai. Đối với các tình huống phức tạp hơn, như xử lý cờ hoặc tùy chọn, Go cung cấp gói `flag` mang lại khả năng phân tích mạnh mẽ. Điều này có thể được coi là một phương án "tốt hơn" khi ứng dụng của bạn yêu cầu nhiều hơn là chỉ các đối số vị trí.

Không giống như một số ngôn ngữ kịch bản cung cấp việc phân tích đối số dòng lệnh thành mảng kết hợp hoặc đối tượng tích hợp, cách tiếp cận của Go yêu cầu lập trình viên hoặc tự xử lý phân tích bằng `os.Args` cho nhu cầu cơ bản hoặc sử dụng gói `flag` cho các tình huống tiên tiến hơn. Thiết kế này phản ánh triết lý của Go về việc giữ cho ngôn ngữ cốt lõi đơn giản trong khi cung cấp các thư viện chuẩn mạnh mẽ cho các tác vụ phổ biến. Mặc dù nó có thể giới thiệu một đường cong học tập nhẹ cho những người quen với việc phân tích tích hợp, nhưng nó mang lại tính linh hoạt lớn hơn và khuyến khích sự hiểu biết sâu sắc hơn về việc xử lý đối số dòng lệnh.
