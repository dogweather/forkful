---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:10.149319-07:00
description: "L\xE0m Th\u1EBF N\xE0o: Vi\u1EC7c t\u1EA1o v\xE0 kh\u1EDFi t\u1EA1o\
  \ m\u1ED9t b\u1EA3n \u0111\u1ED3 trong Go c\xF3 th\u1EC3 \u0111\u01B0\u1EE3c th\u1EF1\
  c hi\u1EC7n b\u1EB1ng nhi\u1EC1u c\xE1ch. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9\
  t v\xED d\u1EE5 c\u01A1 b\u1EA3n \u0111\u1EC3 b\u1EAFt \u0111\u1EA7u."
lastmod: '2024-03-13T22:44:35.971847-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c t\u1EA1o v\xE0 kh\u1EDFi t\u1EA1o m\u1ED9t b\u1EA3n \u0111\u1ED3\
  \ trong Go c\xF3 th\u1EC3 \u0111\u01B0\u1EE3c th\u1EF1c hi\u1EC7n b\u1EB1ng nhi\u1EC1\
  u c\xE1ch."
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
weight: 15
---

## Làm Thế Nào:
Việc tạo và khởi tạo một bản đồ trong Go có thể được thực hiện bằng nhiều cách. Dưới đây là một ví dụ cơ bản để bắt đầu:

```go
package main

import "fmt"

func main() {
    // Khai báo và khởi tạo một bản đồ
    colors := map[string]string{
        "red":   "#FF0000",
        "green": "#00FF00",
        "blue":  "#0000FF",
    }

    fmt.Println(colors)
    // Output: map[blue:#0000FF green:#00FF00 red:#FF0000]
}
```

Để thêm hoặc cập nhật các phần tử, bạn gán một giá trị cho một khóa như sau:

```go
colors["white"] = "#FFFFFF"
fmt.Println(colors)
// Output: map[blue:#0000FF green:#00FF00 red:#FF0000 white:#FFFFFF]
```

Truy cập một giá trị bằng khóa của nó rất đơn giản:

```go
fmt.Println("Mã hex cho màu đỏ là:", colors["red"])
// Output: Mã hex cho màu đỏ là: #FF0000
```

Để xóa một phần tử, sử dụng hàm `delete`:

```go
delete(colors, "red")
fmt.Println(colors)
// Output: map[blue:#0000FF green:#00FF00 white:#FFFFFF]
```

Lặp qua một bản đồ được thực hiện sử dụng vòng lặp for:

```go
for color, hex := range colors {
    fmt.Printf("Khóa: %s Giá Trị: %s\n", color, hex)
}
```

Hãy nhớ, bản đồ trong Go không được sắp xếp. Thứ tự lặp qua không được đảm bảo.

## Đi Sâu Hơn
Trong Go, bản đồ được thực thi như là bảng băm. Mỗi nhập vào trong bản đồ bao gồm hai mục: một khóa và một giá trị. Khóa được băm để lưu trữ nhập vào, điều này cho phép thao tác thời gian hằng số cho một tập hợp dữ liệu nhỏ và độ phức tạp thời gian trung bình là O(1) với băm phù hợp, có thể suy giảm thành O(n) trong trường hợp xấu nhất với nhiều xung đột băm.

Một lưu ý quan trọng cho các lập trình viên mới của Go là loại bản đồ là loại tham chiếu. Điều này có nghĩa là khi bạn đưa một bản đồ vào một hàm, bất kỳ thay đổi nào được thực hiện trên bản đồ bên trong hàm đó đều có thể thấy được bởi caller. Điều này khác với việc, chẳng hạn, đưa một cấu trúc vào một hàm, nơi cấu trúc được sao chép trừ khi được truyền bằng con trỏ.

Mặc dù bản đồ vô cùng linh hoạt và hiệu quả cho hầu hết các trường hợp sử dụng liên quan đến mảng kết hợp, trong các ứng dụng quan trọng về hiệu suất, có thể có lợi khi sử dụng các cấu trúc dữ liệu với đặc tính hiệu suất dễ đoán hơn, đặc biệt nếu phân bố khóa có thể gây ra va chạm thường xuyên.

Một phương án khác cần xem xét là `sync.Map`, có sẵn từ Go 1.9, được thiết kế cho các trường hợp sử dụng nơi khóa chỉ được viết một lần nhưng được đọc nhiều lần, cung cấp cải thiện hiệu quả trong các tình huống này. Tuy nhiên, cho các ứng dụng Go thông thường, việc sử dụng bản đồ thường là cách tiếp cận được khuyến nghị cho sự đơn giản và hỗ trợ trực tiếp trong ngôn ngữ.
