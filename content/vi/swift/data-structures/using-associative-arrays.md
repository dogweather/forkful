---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:41.718322-07:00
description: "M\u1EA3ng k\u1EBFt h\u1EE3p, \u0111\u01B0\u1EE3c bi\u1EBFt \u0111\u1EBF\
  n v\u1EDBi c\xE1i t\xEAn t\u1EEB \u0111i\u1EC3n trong Swift, cho ph\xE9p b\u1EA1\
  n l\u01B0u tr\u1EEF v\xE0 qu\u1EA3n l\xFD d\u1EEF li\u1EC7u d\u01B0\u1EDBi d\u1EA1\
  ng c\u1EB7p kh\xF3a-gi\xE1 tr\u1ECB. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng\u2026"
lastmod: 2024-02-19 22:04:56.293431
model: gpt-4-0125-preview
summary: "M\u1EA3ng k\u1EBFt h\u1EE3p, \u0111\u01B0\u1EE3c bi\u1EBFt \u0111\u1EBF\
  n v\u1EDBi c\xE1i t\xEAn t\u1EEB \u0111i\u1EC3n trong Swift, cho ph\xE9p b\u1EA1\
  n l\u01B0u tr\u1EEF v\xE0 qu\u1EA3n l\xFD d\u1EEF li\u1EC7u d\u01B0\u1EDBi d\u1EA1\
  ng c\u1EB7p kh\xF3a-gi\xE1 tr\u1ECB. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng\u2026"
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
---

{{< edit_this_page >}}

## Gì và Tại sao?

Mảng kết hợp, được biết đến với cái tên từ điển trong Swift, cho phép bạn lưu trữ và quản lý dữ liệu dưới dạng cặp khóa-giá trị. Lập trình viên sử dụng chúng để tổ chức dữ liệu một cách hiệu quả, làm cho việc truy cập và thao tác với giá trị dựa trên khóa duy nhất trở nên dễ dàng hơn.

## Làm thế nào:

Swift làm cho việc làm việc với mảng kết hợp trở nên đơn giản. Dưới đây là cách bạn có thể khai báo, thêm, xóa và truy cập các mục trong từ điển Swift:

```Swift
// Khai báo một từ điển
var fruitColors: [String: String] = ["Apple": "Red", "Banana": "Yellow"]

// Thêm một mục mới
fruitColors["Grape"] = "Purple"

// Truy cập một giá trị sử dụng khóa của nó
if let appleColor = fruitColors["Apple"] {
    print("Apple là \(appleColor).")  // Đầu ra: Apple là Red.
} else {
    print("Không tìm thấy màu sắc.")
}

// Xóa một mục
fruitColors["Banana"] = nil  // Điều này sẽ xóa "Banana" khỏi từ điển

// Duyệt qua các mục
for (fruit, color) in fruitColors {
    print("\(fruit) là \(color).")
    // Đầu ra:
    // Apple là Red.
    // Grape là Purple.
}
```

Từ điển cực kỳ linh hoạt, cho phép bạn thao tác và truy cập dữ liệu một cách động. Bản chất không có thứ tự của chúng không ảnh hưởng đến tốc độ truy xuất dữ liệu, đây là một lợi ích đáng kể khi xử lý các bộ dữ liệu lớn.

## Sâu hơn

Việc Swift triển khai từ điển như một mảng kết hợp xuất phát từ khả năng mạnh mẽ của chúng trong việc ánh xạ khóa duy nhất với giá trị. Trong lịch sử, các ngôn ngữ lập trình đã triển khai khái niệm này dưới nhiều tên gọi khác nhau như bảng băm hoặc bản đồ, ngụ ý về chức năng của chúng trong việc tạo ra một "bản đồ" giữa khóa và giá trị.

Trong Swift, từ điển được tối ưu hóa cho hiệu suất, tận dụng khóa có thể băm để truy xuất dữ liệu một cách hiệu quả. Điều này có nghĩa là kiểu `Key` trong một từ điển `[Key: Value]` phải tuân thủ giao thức `Hashable`, đúng với hầu hết các kiểu tiêu chuẩn của Swift như `Int`, `String` và `Double`.

Một điều cần xem xét là mặc dù từ điển rất tốt để kết hợp cặp dữ liệu, chúng thiếu thứ tự. Nếu bạn cần duy trì thứ tự của các phần tử, bạn có thể khám phá các lựa chọn thay thế như `Array` cho một chuỗi các phần tử có thứ tự hoặc các cấu trúc dữ liệu tùy chỉnh kết hợp các đặc điểm của cả mảng và từ điển.

Điều đáng chú ý là Swift không ngừng phát triển, và việc xử lý cũng như tối ưu hóa từ điển của nó cũng vậy. Do đó, việc cập nhật với tài liệu Swift mới nhất là rất quan trọng để tận dụng tối đa từ điển, đảm bảo bạn đang sử dụng các thực hành hiệu quả và cập nhật nhất.
