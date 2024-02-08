---
title:                "Viết kiểm thử"
aliases:
- vi/go/writing-tests.md
date:                  2024-02-03T18:15:36.974857-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết kiểm thử"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Viết test trong Go bao gồm việc tạo ra những đoạn mã nhỏ, dễ quản lý nhằm xác nhận chức năng và hành vi của ứng dụng. Lập trình viên viết test để đảm bảo mã của họ hoạt động như mong đợi dưới các điều kiện khác nhau, hỗ trợ tái cấu trúc và giúp ngăn chặn sự quay lại của các lỗi đã sửa.

## Làm thế nào:

Trong Go, test thường được viết trong cùng một gói với mã họ kiểm tra. Các file chứa test được đặt tên với hậu tố `_test.go`. Test là những hàm nhận một con trỏ đến đối tượng testing.T (từ gói `testing`) làm đối số, và chúng báo hiệu lỗi bằng cách gọi các phương thức như `t.Fail()`, `t.Errorf()`, v.v.

Ví dụ về một test đơn giản cho hàm `Add` được định nghĩa trong `math.go`:
```go
// math.go
package math

func Add(x, y int) int {
    return x + y
}
```

File test `math_test.go`:
```go
package math

import "testing"

func TestAdd(t *testing.T) {
    result := Add(1, 2)
    expected := 3
    if result != expected {
        t.Errorf("Add(1, 2) = %d; muốn %d", result, expected)
    }
}
```

Chạy test của bạn với lệnh `go test` trong cùng thư mục với các file test. Đầu ra mẫu cho tín hiệu một test đã qua sẽ trông giống như:

```
PASS
ok      example.com/my/math 0.002s
```

Đối với các test dựa trên bảng, cho phép bạn kiểm tra hiệu quả các tổ hợp đầu vào và đầu ra khác nhau, hãy định nghĩa một slice của các struct đại diện cho các trường hợp test:

```go
func TestAddTableDriven(t *testing.T) {
    var tests = []struct {
        x        int
        y        int
        expected int
    }{
        {1, 2, 3},
        {2, 3, 5},
        {-1, -2, -3},
    }

    for _, tt := range tests {
        testname := fmt.Sprintf("%d+%d", tt.x, tt.y)
        t.Run(testname, func(t *testing.T) {
            ans := Add(tt.x, tt.y)
            if ans != tt.expected {
                t.Errorf("được %d, muốn %d", ans, tt.expected)
            }
        })
    }
}
```

## Sâu hơn nữa

Bộ khung kiểm tra Go, ra mắt trong Go 1 cùng với chính ngôn ngữ, được thiết kế để tích hợp liền mạch với bộ công cụ Go, phản ánh nhấn mạnh của Go vào sự đơn giản và hiệu quả trong phát triển phần mềm. Khác với một số khung kiểm tra trong các ngôn ngữ khác dựa trên thư viện bên ngoài hay cài đặt phức tạp, gói `testing` được tích hợp sẵn của Go cung cấp một cách thẳng thắn để viết và chạy test.

Một điểm thú vị của cách tiếp cận kiểm tra của Go là nguyên tắc chọn lựa trước cấu hình mà nó áp dụng, như mô hình đặt tên file (`_test.go`) và việc sử dụng các chức năng của thư viện chuẩn hơn là các phụ thuộc bên ngoài. Cách tiếp cận tối giản này khuyến khích các nhà phát triển viết test, vì rào cản để bắt đầu thấp.

Mặc dù các tiện ích kiểm tra được tích hợp sẵn của Go bao trùm nhiều lĩnh vực, có những tình huống mà các công cụ hoặc khung công tác bên thứ ba có thể cung cấp nhiều chức năng hơn, như sinh mock, kiểm tra fuzz, hoặc kiểm tra theo phong cách phát triển dựa trên hành vi (BDD). Các thư viện phổ biến như Testify hoặc GoMock bổ sung cho khả năng kiểm tra tiêu chuẩn của Go, cung cấp các phát biểu biểu cảm hơn hoặc khả năng tạo mock, có thể đặc biệt hữu ích trong các ứng dụng phức tạp với nhiều phụ thuộc.

Mặc dù có sự tồn tại của các lựa chọn thay thế này, gói kiểm tra tiêu chuẩn của Go vẫn là cốt lõi cho việc kiểm tra trong Go do sự đơn giản, hiệu suất và tích hợp chặt chẽ với ngôn ngữ và bộ công cụ. Cho dù các nhà phát triển chọn bổ sung nó với các công cụ bên thứ ba hay không, khung kiểm tra Go cung cấp một nền tảng vững chắc cho việc bảo đảm chất lượng mã và độ tin cậy.
