---
title:                "Sử dụng mảng liên kết"
date:                  2024-01-30T19:12:00.789796-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng mảng liên kết"

category:             "Go"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/go/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?

Mảng kết hợp, được biết đến với cái tên maps trong Go, cho phép bạn lưu trữ và truy cập dữ liệu với các cặp khóa-giá trị. Chúng là cần thiết cho việc quản lý các bộ sưu tập nơi bạn có thể nhanh chóng tìm kiếm giá trị bằng một khóa duy nhất, đơn giản hóa việc thao tác và truy xuất dữ liệu trong các chương trình của bạn.

## Cách thực hiện:

Trong Go, maps rất dễ sử dụng. Dưới đây là hướng dẫn đơn giản để bắt đầu:

1. **Khai báo và Khởi tạo Maps**

```Go
package main

import "fmt"

func main() {
    // Khởi tạo một map rỗng với khóa kiểu string và giá trị kiểu int
    var scores map[string]int
    fmt.Println(scores) // In: map[]

    // Khai báo và khởi tạo một map không rỗng
    colors := map[string]string{
        "red": "#ff0000",
        "green": "#00ff00",
    }
    fmt.Println(colors) // In: map[green:#00ff00 red:#ff0000]
}
```

2. **Thêm và Truy cập Phần tử**

```Go
func main() {
    fruits := make(map[string]int)
    fruits["apples"] = 5
    fruits["bananas"] = 10

    fmt.Println(fruits["apples"]) // In: 5
}
```

3. **Duyệt qua Maps**

```Go
func main() {
    pets := map[string]string{"dog": "bark", "cat": "meow"}

    for key, value := range pets {
        fmt.Printf("%s goes %s\n", key, value)
    }
    // Thứ tự đầu ra có thể thay đổi, do maps không đảm bảo thứ tự.
}
```

4. **Xóa Phần tử**

```Go
func main() {
    meals := map[string]int{"breakfast": 300, "lunch": 600}
    fmt.Println(meals) // Trước khi xóa

    delete(meals, "lunch")
    fmt.Println(meals) // Sau khi xóa
}
```

## Sâu hơn

Được giới thiệu trong Go 1, maps cung cấp một cách tích hợp sẵn để xử lý các mảng kết hợp một cách hiệu quả. Khác với slices, là các bộ sưu tập có thứ tự, maps là không có thứ tự. Điều này có nghĩa là thứ tự lặp qua các phần tử của map không được đảm bảo giống nhau qua các lần thực thi, là một sự đánh đổi cho khả năng xử lý các cặp khóa-giá trị một cách linh hoạt và động.

Bên dưới lớp vỏ, Go thực thi maps dưới dạng bảng băm, đảm bảo độ phức tạp trung bình của các thao tác truy cập, chèn, và xoá là O(1), trong hầu hết các trường hợp. Tuy nhiên, đáng lưu ý là hiệu quả này có thể biến đổi dựa trên các yếu tố như va chạm băm.

Đối với các trường hợp sử dụng yêu cầu duyệt qua khóa có thứ tự, bạn có thể xem xét kết hợp maps với slices hoặc khám phá các gói bên thứ ba cung cấp các cấu trúc dữ liệu bổ sung như maps có thứ tự hoặc cây. Mặc dù có những hạn chế, maps của Go vẫn là một công cụ mạnh mẽ và cần thiết cho nhiều tình huống lập trình.
