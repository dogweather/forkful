---
title:                "Đọc một tệp văn bản"
date:                  2024-01-28T22:05:17.448149-07:00
model:                 gpt-4-0125-preview
simple_title:         "Đọc một tệp văn bản"

category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/reading-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Đọc một tệp văn bản là việc lấy dữ liệu đã được lưu trữ bên trong tệp trên đĩa của bạn. Lập trình viên thực hiện điều này để xử lý các log, cấu hình, dữ liệu người dùng - mọi thứ bạn có thể nghĩ đến - bởi vì đó thường là nơi hoạt động diễn ra: dữ liệu.

## Làm thế nào:

Đọc một tệp trong Go khá đơn giản. Sử dụng gói `ioutil` cho một giải pháp nhanh chóng, hoặc chọn `os` và `bufio` để có nhiều quyền kiểm soát hơn. Dưới đây là cách dùng `ioutil`, dễ như chơi:

```Go
package main

import (
    "fmt"
    "io/ioutil"
)

func main() {
    data, err := ioutil.ReadFile("example.txt")
    if err != nil {
        panic(err)
    }
    fmt.Println(string(data))
}
```

Để có nhiều kỹ thuật tinh vi hơn, hãy bắt tay vào làm với `os` và `bufio`:

```Go
package main

import (
    "bufio"
    "fmt"
    "os"
)

func main() {
    file, err := os.Open("example.txt")
    if err != nil {
        panic(err)
    }
    defer file.Close()

    scanner := bufio.NewScanner(file)
    for scanner.Scan() {
        fmt.Println(scanner.Text())
    }

    if err := scanner.Err(); err != nil {
        panic(err)
    }
}
```

Trong cả hai trường hợp, thay "example.txt" bằng tên tệp của bạn. Chạy code, và nó sẽ phát ra nội dung của tệp.

## Sâu hơn nữa

Ban đầu, `ioutil.ReadFile` của Go là lựa chọn phổ biến cho việc đọc tệp nhanh chóng. Đó là một dòng lệnh, nhưng nó đọc toàn bộ tệp cùng một lúc. Điều này không lý tưởng cho các tệp văn bản lớn nơi bộ nhớ là mối quan tâm.

Nhập `os` và `bufio`. Chúng cho phép bạn truyền tệp, xử lý từng dòng một. Điều này có nghĩa là bạn có thể xử lý hàng gigabyte mà không hề gặp khó khăn (hoặc làm hỏng ứng dụng của bạn).

Có lựa chọn khác không? Chắc chắn rồi. Có các gói như `afero` cho một giao diện hệ thống tệp nhất quán, có thể hữu ích cho việc kiểm thử.

Một chút chi tiết triển khai: `bufio.Scanner` có kích thước token tối đa mặc định (thường là một dòng), vì vậy những dòng siêu dài có thể cần xử lý đặc biệt. Điều chỉnh nó bằng `scanner.Buffer()` nếu bạn gặp trường hợp ngoại lệ này.

## Xem thêm

- Để đào sâu vào chi tiết, hãy kiểm tra tài liệu gói của Go [ioutil](https://pkg.go.dev/io/ioutil), [os](https://pkg.go.dev/os), và [bufio](https://pkg.go.dev/bufio).
- Tò mò về `afero`? Dưới đây là [GitHub repo](https://github.com/spf13/afero).
