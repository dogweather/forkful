---
title:                "Xử lý lỗi"
aliases:
- /vi/go/handling-errors.md
date:                  2024-02-03T17:59:10.206674-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xử lý lỗi"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/handling-errors.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Xử lý lỗi trong Go bao gồm việc nhận biết và phản hồi trạng thái lỗi trong chương trình của bạn. Lập trình viên thực hiện xử lý lỗi để đảm bảo ứng dụng của họ có thể phục hồi một cách suôn sẻ từ các tình huống không mong muốn, dẫn tới phần mềm ổn định và đáng tin cậy hơn.

## Làm thế nào:

Trong Go, việc xử lý lỗi được quản lý một cách rõ ràng sử dụng kiểu `error`. Các hàm có thể thất bại sẽ trả về một lỗi như giá trị trả về cuối cùng của họ. Kiểm tra xem giá trị lỗi này có phải là `nil` hay không sẽ cho bạn biết liệu có lỗi xảy ra hay không.

```go
package main

import (
    "errors"
    "fmt"
)

func Compute(value int) (int, error) {
    if value > 100 {
        return 0, errors.New("giá trị phải là 100 hoặc thấp hơn")
    }
    return value * 2, nil
}

func main() {
    result, err := Compute(150)
    if err != nil {
        fmt.Println("Lỗi:", err)
    } else {
        fmt.Println("Kết quả:", result)
    }
    
    // Xử lý một lỗi một cách nhẹ nhàng
    anotherResult, anotherErr := Compute(50)
    if anotherErr != nil {
        fmt.Println("Lỗi:", anotherErr)
    } else {
        fmt.Println("Kết quả:", anotherResult)
    }
}
```

Đầu ra mẫu cho đoạn code trên:
```
Lỗi: giá trị phải là 100 hoặc thấp hơn
Kết quả: 100
```

Trong ví dụ này, hàm `Compute` hoặc là trả về một giá trị đã tính toán hoặc một lỗi. Người gọi xử lý lỗi bằng cách kiểm tra xem `err` có phải là `nil` không.

## Sâu hơn

Cách tiếp cận xử lý lỗi của Go cố ý đơn giản và an toàn về kiểu dữ liệu, đòi hỏi phải kiểm tra lỗi một cách rõ ràng. Khái niệm này trái ngược với xử lý lỗi dựa trên ngoại lệ thấy trong các ngôn ngữ như Java và Python, nơi lỗi được truyền lên ngăn xếp gọi trừ khi được bắt bởi trình xử lý ngoại lệ. Nhóm Go cho rằng việc xử lý lỗi một cách rõ ràng dẫn đến mã nguồn rõ ràng và đáng tin cậy hơn, vì nó buộc lập trình viên phải giải quyết lỗi ngay tại nơi chúng xảy ra.

Tuy nhiên, một số chỉ trích cho rằng mô hình này có thể dẫn đến mã nguồn dài dòng, đặc biệt trong các hàm phức tạp với nhiều hoạt động dễ gặp lỗi. Đáp lại, các phiên bản Go mới hơn đã giới thiệu các tính năng xử lý lỗi tinh vi hơn, như gói lỗi, giúp dễ dàng cung cấp ngữ cảnh cho một lỗi mà không mất thông tin lỗi gốc. Cộng đồng cũng đã thấy các đề xuất cho các cơ chế xử lý lỗi mới, như kiểm tra/xử lý, mặc dù những đề xuất này vẫn đang được thảo luận tính đến lần cập nhật cuối cùng của tôi.

Triết lý xử lý lỗi của Go nhấn mạnh sự hiểu biết và lập kế hoạch cho lỗi là một phần của dòng chảy bình thường của chương trình. Cách tiếp cận này khuyến khích việc phát triển phần mềm dễ dự đoán và kiên cường hơn, mặc dù có thể gia tăng mã boilerplate. Các mô hình và thư viện thay thế tồn tại để đơn giản hóa việc xử lý lỗi trong các trường hợp đặc biệt phức tạp, nhưng kiểu `error` được tích hợp sẵn của Go vẫn là nền tảng của việc xử lý lỗi trong ngôn ngữ.
