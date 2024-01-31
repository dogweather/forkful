---
title:                "Viết các bài kiểm tra"
date:                  2024-01-28T22:13:15.725855-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết các bài kiểm tra"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Viết kiểm thử có nghĩa là tạo ra mã để kiểm tra xem mã khác có hoạt động không. Lập trình viên làm điều này để phát hiện lỗi sớm, đảm bảo chức năng và tránh những đau khổ trong tương lai.

## Làm thế nào:

Go có một gói kiểm thử tích hợp sẵn có tên là `testing`. Để minh họa, giả sử bạn có một hàm `Add` tính tổng hai số nguyên:

```Go
// add.go
package math

func Add(x, y int) int {
    return x + y
}
```

Viết một bài kiểm thử như sau:

```Go
// add_test.go
package math

import (
    "testing"
)

func TestAdd(t *testing.T) {
    kết_quả := Add(1, 2)
    mong_đợi := 3
    if kết_quả != mong_đợi {
        t.Errorf("Add(1, 2) = %d; muốn %d", kết_quả, mong_đợi)
    }
}
```

Chạy kiểm thử với `go test`. Bạn sẽ thấy kết quả như sau:

```
PASS
ok      example.com/your-module/math   0.002s
```

## Sâu hơn nữa

Go đã giới thiệu việc kiểm thử tích hợp sẵn vào năm 2011. Nó đơn giản hơn so với việc sử dụng một thư viện riêng biệt. Bạn viết các bài kiểm thử trong các tệp `_test.go`, sử dụng `testing.T` để báo cáo thất bại.

Có lựa chọn khác? Chắc chắn, bạn có thể sử dụng Testify cho các phát biểu, Ginkgo cho BDD, hoặc GoCheck cho các tính năng nâng cao hơn. Nhưng gói `testing` không có sự phụ thuộc, dễ dàng, và thường là đủ.

Dưới nắp capo, `go test` biên dịch mã và kiểm thử của bạn cùng nhau, chạy chúng và báo cáo kết quả. Đó là cách Go: trường hợp phổ biến dễ dàng, trường hợp đặc biệt khả thi.

## Xem thêm

Để biết thêm thông tin, hãy kiểm tra tài liệu:

- Gói kiểm thử: [https://pkg.go.dev/testing](https://pkg.go.dev/testing)
- Kiểm thử dựa trên bảng: [https://github.com/golang/go/wiki/TableDrivenTests](https://github.com/golang/go/wiki/TableDrivenTests)
