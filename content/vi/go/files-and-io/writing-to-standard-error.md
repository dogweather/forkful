---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:15:33.281762-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Go, g\xF3i `os` cung c\u1EA5p gi\xE1 tr\u1ECB\
  \ `Stderr`, \u0111\u1EA1i di\u1EC7n cho t\u1EC7p l\u1ED7i chu\u1EA9n. B\u1EA1n c\xF3\
  \ th\u1EC3 s\u1EED d\u1EE5ng n\xF3 v\u1EDBi c\xE1c h\xE0m `fmt.Fprint`, `fmt.Fprintf`,\
  \ ho\u1EB7c\u2026"
lastmod: '2024-03-13T22:44:36.004655-06:00'
model: gpt-4-0125-preview
summary: "Trong Go, g\xF3i `os` cung c\u1EA5p gi\xE1 tr\u1ECB `Stderr`, \u0111\u1EA1\
  i di\u1EC7n cho t\u1EC7p l\u1ED7i chu\u1EA9n."
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
weight: 25
---

## Làm thế nào:
Trong Go, gói `os` cung cấp giá trị `Stderr`, đại diện cho tệp lỗi chuẩn. Bạn có thể sử dụng nó với các hàm `fmt.Fprint`, `fmt.Fprintf`, hoặc `fmt.Fprintln` để viết vào stderr. Dưới đây là một ví dụ đơn giản:

```go
package main

import (
    "fmt"
    "os"
)

func main() {
    // Viết một chuỗi đơn giản vào stderr
    _, err := fmt.Fprintln(os.Stderr, "Đây là một thông báo lỗi!")
    if err != nil {
        panic(err)
    }

    // Thông báo lỗi được định dạng với Fprintf
    errCount := 4
    _, err = fmt.Fprintf(os.Stderr, "Quá trình hoàn thành với %d lỗi.\n", errCount)
    if err != nil {
        panic(err)
    }
}
```

Đầu ra mẫu (vào stderr):
```
Đây là một thông báo lỗi!
Quá trình hoàn thành với 4 lỗi.
```

Nhớ rằng, các thông báo này sẽ không xuất hiện trong đầu ra thông thường (stdout) mà trong dòng lỗi, có thể được chuyển hướng riêng biệt trong hầu hết các hệ điều hành.

## Sâu hơn
Khái niệm về lỗi chuẩn có nguồn gốc sâu sắc trong triết lý Unix, phân biệt rõ ràng giữa đầu ra bình thường và thông báo lỗi để xử lý và quản lý dữ liệu một cách hiệu quả hơn. Trong Go, quy ước này được chấp nhận thông qua gói `os`, cung cấp trực tiếp quyền truy cập vào các bộ điều khiển tệp stdin, stdout và stderr.

Trong khi viết trực tiếp vào `os.Stderr` phù hợp với nhiều ứng dụng, Go cũng cung cấp các gói đăng nhập chi tiết hơn như `log`, cung cấp các tính năng bổ sung như dấu thời gian và cấu hình đầu ra linh hoạt hơn (ví dụ: viết vào tệp). Sử dụng gói `log`, đặc biệt cho các ứng dụng lớn hơn hoặc nơi cần có các tính năng đăng nhập toàn diện hơn, có thể là một lựa chọn tốt hơn. Cũng đáng chú ý là cách tiếp cận của Go đối với việc xử lý lỗi, khuyến khích trả về lỗi từ các hàm, bổ sung cho thực hành viết thông báo lỗi vào stderr, cho phép kiểm soát lỗi và báo cáo lỗi một cách chi tiết hơn.

Về bản chất, trong khi viết vào stderr là một nhiệm vụ cơ bản trong nhiều ngôn ngữ lập trình, thư viện tiêu chuẩn của Go và các nguyên tắc thiết kế cung cấp cả con đường trực tiếp và tiên tiến để quản lý đầu ra lỗi, phù hợp với các thực hành rộng lớn trong ngành trong khi cũng phục vụ cho tinh thần thiết kế đặc biệt của Go.
