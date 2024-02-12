---
title:                "Ghi vào lỗi chuẩn"
aliases:
- /vi/go/writing-to-standard-error/
date:                  2024-02-03T18:15:33.281762-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi vào lỗi chuẩn"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc viết vào lỗi chuẩn (stderr) trong Go bao gồm việc chỉ đạo các thông báo lỗi hoặc chẩn đoán không dành cho dòng xuất chính. Các lập trình viên sử dụng điều này để phân tách đầu ra thông thường khỏi thông tin lỗi, làm cho việc gỡ lỗi và phân tích nhật ký trở nên đơn giản hơn.

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
