---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:15:33.281762-07:00
description: "Vi\u1EC7c vi\u1EBFt v\xE0o l\u1ED7i chu\u1EA9n (stderr) trong Go bao\
  \ g\u1ED3m vi\u1EC7c ch\u1EC9 \u0111\u1EA1o c\xE1c th\xF4ng b\xE1o l\u1ED7i ho\u1EB7\
  c ch\u1EA9n \u0111o\xE1n kh\xF4ng d\xE0nh cho d\xF2ng xu\u1EA5t ch\xEDnh. C\xE1\
  c l\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng\u2026"
lastmod: '2024-03-13T22:44:36.004655-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c vi\u1EBFt v\xE0o l\u1ED7i chu\u1EA9n (stderr) trong Go bao g\u1ED3\
  m vi\u1EC7c ch\u1EC9 \u0111\u1EA1o c\xE1c th\xF4ng b\xE1o l\u1ED7i ho\u1EB7c ch\u1EA9\
  n \u0111o\xE1n kh\xF4ng d\xE0nh cho d\xF2ng xu\u1EA5t ch\xEDnh."
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
