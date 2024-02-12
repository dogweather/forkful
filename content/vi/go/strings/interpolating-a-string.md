---
title:                "Nội suy một chuỗi ký tự"
aliases: - /vi/go/interpolating-a-string.md
date:                  2024-02-03T17:58:49.660019-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nội suy một chuỗi ký tự"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/interpolating-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?

Nội suy chuỗi là một phương pháp để xây dựng chuỗi có chứa biến, cho phép tạo chuỗi động. Lập trình viên làm điều này để tùy chỉnh thông điệp, xây dựng URL, tạo truy vấn SQL, và nhiều hơn nữa, giúp mã dễ đọc và bảo trì hơn.

## Làm thế nào:

Trong Go, nội suy chuỗi thường được thực hiện bằng cách sử dụng gói `fmt`, đặc biệt là với hàm `Sprintf`, cho phép bạn chèn biến vào chuỗi bằng cách chỉ rõ các động từ định dạng. Các động từ là các trình giữ chỗ trong chuỗi định dạng và được thay thế bằng giá trị của các biến đã cho. Đây là cách bạn sử dụng nó:

```go
package main

import (
    "fmt"
)

func main() {
    name := "Jane"
    age := 28

    // Sử dụng Sprintf cho nội suy chuỗi
    message := fmt.Sprintf("Xin chào, tên tôi là %s và tôi %d tuổi.", name, age)
    fmt.Println(message) // Đầu ra: Xin chào, tên tôi là Jane và tôi 28 tuổi.
}
```

Lưu ý rằng `%s` được sử dụng cho chuỗi, và `%d` cho số nguyên. Tài liệu của gói `fmt` cung cấp một danh sách toàn diện về các động từ định dạng cho các loại dữ liệu khác nhau.

## Sâu hơn

Khái niệm về nội suy chuỗi tồn tại trong nhiều ngôn ngữ lập trình, mặc dù với các cú pháp và khả năng khác nhau. Trong Go, mặc dù hàm `Sprintf` của gói `fmt` là phương pháp thường được sử dụng nhất, nó có thể không phải là phương pháp hiệu quả nhất, đặc biệt là đối với các nối chuỗi đơn giản hoặc khi làm việc trong mã với yêu cầu hiệu suất cao.

Gói `fmt` sử dụng phản xạ để động giải thích các loại của biến tại thời gian chạy, có tính linh hoạt nhưng gây ra tải. Trong các tình huống mà hiệu suất là quan trọng, nối chuỗi trực tiếp hoặc kiểu `strings.Builder` có thể cung cấp các lựa chọn tốt hơn. Nối chuỗi trực tiếp thì đơn giản nhưng có thể trở nên khó quản lý với nhiều biến. `strings.Builder`, mặt khác, cung cấp một cách hiệu suất cao và dễ đọc hơn để xây dựng chuỗi phức tạp trong một vòng lặp hoặc khi xử lý nhiều biến:

```go
var sb strings.Builder
sb.WriteString("Xin chào, tên tôi là ")
sb.WriteString(name)
sb.WriteString(" và tôi ")
sb.WriteString(strconv.Itoa(age))
sb.WriteString(" tuổi.")
message := sb.String()

fmt.Println(message) // Đầu ra giống như trước
```

Cuối cùng, sự lựa chọn giữa `fmt.Sprintf`, nối chuỗi trực tiếp, và `strings.Builder` phụ thuộc vào các yêu cầu cụ thể của ứng dụng của bạn, như độ phức tạp của chuỗi được xây dựng và các xem xét về hiệu suất.
