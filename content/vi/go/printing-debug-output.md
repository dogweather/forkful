---
title:                "In ra thông tin gỡ lỗi"
date:                  2024-01-28T22:04:53.951799-07:00
model:                 gpt-4-0125-preview
simple_title:         "In ra thông tin gỡ lỗi"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/printing-debug-output.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
In thông báo để gỡ lỗi là để xuất dữ liệu ra ngoài để kiểm tra xem đoạn code của bạn đang làm gì. Các lập trình viên thực hiện việc này để tìm và sửa lỗi hoặc để hiểu rõ luồng và trạng thái dữ liệu chỉ qua một cái nhìn.

## Làm thế nào:
Dưới đây là cách để thêm một số dòng in vào code Go của bạn.

```Go
package main

import (
    "fmt"
    "log"
)

func main() {
    // In cơ bản ra stdout
    fmt.Println("Xin chào, tôi là một câu lệnh in!")

    // In có định dạng
    name, age := "Jane", 28
    fmt.Printf("%s là %d tuổi.\n", name, age)

    // In cùng log (bao gồm dấu thời gian)
    log.Println("Đây là thông tin log có dấu thời gian.")

    // Đối với việc gỡ lỗi, sử dụng Printf, nhưng nhớ loại bỏ sau này
    debug := true
    if debug {
        fmt.Printf("Thông tin gỡ lỗi: %s là %d tuổi.\n", name, age)
    }
}
```

Kết quả mẫu:
```
Xin chào, tôi là một câu lệnh in!
Jane là 28 tuổi.
2009/11/10 23:00:00 Đây là thông tin log có dấu thời gian.
Thông tin gỡ lỗi: Jane là 28 tuổi.
```

## Đào sâu:
Theo lịch sử, `fmt` là lựa chọn hàng đầu của Go cho các hoạt động I/O kể từ khi nó được tạo ra. Nó viết tắt của 'format' và cung cấp một loạt các hàm để chỉnh sửa đầu ra văn bản. `Println` và `Printf` là những công cụ chính ở đây. Gói `log` thêm thời gian, phù hợp cho việc theo dõi các sự kiện theo thời gian.

Có sự thay thế không? Chắc chắn rồi, ngoài các câu lệnh in cơ bản, bạn có thể sử dụng các framework log như `logrus` hoặc `zap` cho việc log có cấu trúc và phân cấp, hoàn hảo cho những ứng dụng nghiêm túc.

Phần thực hiện? `fmt` là an toàn với thread, làm cho việc in lỗi gỡ lỗi từ các goroutines đồng thời dễ hiểu. Nhưng cẩn thận, in để gỡ lỗi có thể tốt cho việc kiểm tra nhanh nhưng có thể làm bạn chậm lại hoặc tạo ra hỗn loạn trong mã sản phẩm.

## Xem thêm:
- Go by Example về `fmt`: https://gobyexample.com/fmt
- Blog của Go về "Sử dụng Go Modules": https://blog.golang.org/using-go-modules (kiểm tra phần về các phụ thuộc được cung cấp)
- Tài liệu Go cho `log`: https://pkg.go.dev/log
- Logging có cấu trúc trong Go với `logrus`: https://github.com/sirupsen/logrus
- Logging có cấu trúc, phân cấp, nhanh chóng trong Go với `zap`: https://github.com/uber-go/zap
