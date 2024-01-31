---
title:                "Tạo một tập tin tạm thời"
date:                  2024-01-28T21:58:56.900587-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tạo một tập tin tạm thời"

category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/creating-a-temporary-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Là gì & Tại sao?

Trong lập trình, việc tạo một file tạm thời có nghĩa là tạo một file được dùng cho việc sử dụng ngắn hạn, thường là làm không gian trung gian hoặc bộ đệm. Các lập trình viên thực hiện điều này cho các tác vụ như lưu trữ dữ liệu không cần duy trì, quản lý việc tải lên trước khi xử lý, hoặc chia nhỏ công việc lớn thành các phần nhỏ hơn, dễ quản lý hơn.

## Cách thực hiện:

Dưới đây là cách nhanh chóng và dễ dàng để tạo một file tạm trong Go:

```Go
package main

import (
    "fmt"
    "io/ioutil"
    "os"
)

func main() {
    // Tạo file tạm
    tmpFile, err := ioutil.TempFile("", "example")
    if err != nil {
        panic(err)
    }
    fmt.Println("File Đã Tạo:", tmpFile.Name())
    
    // Dọn dẹp: xóa file sau khi bạn hoàn thành
    defer os.Remove(tmpFile.Name())

    // Ghi điều gì đó vào file
    content := []byte("nội dung của file tạm")
    if _, err = tmpFile.Write(content); err != nil {
        panic(err)
    }
    
    // Nhớ đóng file lại!
    if err := tmpFile.Close(); err != nil {
        panic(err)
    }
}
```

Khi bạn chạy đoạn code này, nó xuất ra tên của file tạm. Như là: `File Đã Tạo: /tmp/example123456`. Mỗi lần chạy, phần `example123456` sẽ thay đổi, đảm bảo tính duy nhất.

## Khoảng sâu:

Truyền thống, file tạm là chìa khóa để quản lý các bước trung gian trong xử lý dữ liệu. Chúng cung cấp một không gian an toàn cho việc thử nghiệm và sai sót mà không gây rủi ro làm hỏng bộ dữ liệu gốc.

Sự thật nhanh: Hệ thống Unix truyền thống sử dụng `/tmp` cho việc lưu trữ tạm thời, và Windows sử dụng `%TEMP%`. Go che điều này đi - `ioutil.TempFile` sử dụng thư mục tạm mà hệ điều hành của bạn chỉ định.

Nếu bạn thắc mắc: có, có những phương án thay thế cho `ioutil.TempFile`. Bạn có thể tự tạo và quản lý file tạm, điều này cho bạn nhiều kiểm soát hơn nhưng cũng đồng nghĩa với rủi ro nhiều lỗi hơn.

Về triển khai, `ioutil.TempFile` tạo tên file duy nhất với một chuỗi ngẫu nhiên, giảm đáng kể khả năng xung đột tên, điều này có thể là một cơn đau đầu thực sự nếu bạn đang xử lý nhiều dữ liệu cùng một lúc.

Nhớ sử dụng `defer` để dọn dẹp sau khi bạn. File tạm được định là tạm thời, cuối cùng, và bạn không muốn để lại lộn xộn cho hệ thống của mình phải xử lý sau này.

## Xem Thêm

- Tài liệu của Go về gói `ioutil`: [gói ioutil - io/ioutil - pkg.go.dev](https://pkg.go.dev/io/ioutil)
- Go by Example: File và Thư mục Tạm: [Go by Example - Temp Files and Directories](https://gobyexample.com/temporary-files-and-directories)
- Effective Go cho các phương pháp hay nhất: [Effective Go - golang.org](https://golang.org/doc/effective_go)
