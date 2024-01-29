---
title:                "Nội suy chuỗi ký tự"
date:                  2024-01-28T22:02:15.640505-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nội suy chuỗi ký tự"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

Nội suy chuỗi là quá trình nhúng các biểu thức vào trong các chuỗi ký tự để tạo ra những giá trị chuỗi mới. Các lập trình viên nội suy chuỗi để xây dựng động các thông điệp, thường là cho đầu ra người dùng hoặc ghi lại.

## Làm thế nào:

Trong Gleam, nội suy chuỗi rất đơn giản. Sử dụng cú pháp `#{}` để chèn các biểu thức vào chuỗi. Dưới đây là một ví dụ nhanh:

```gleam
fn main() {
  let name = "world"
  let greeting = "Hello, #{name}!"
  greeting
}

// Điều này sẽ đầu ra: "Hello, world!"
```

## Sâu xa hơn

Trong lịch sử, ghép nối chuỗi là phổ biến, nơi bạn sẽ tự ghép nối chuỗi và giá trị thủ công. Nó trở nên rối nhanh chóng. Nội suy là sạch sẽ hơn và dễ đọc hơn.

Các ngôn ngữ khác nhau trong cú pháp nội suy của mình; Cú pháp `#{}` của Gleam phản ánh của Ruby và Elixir. Sự nhất quán này hữu ích cho những người nhảy qua các ngôn ngữ.

Bên dưới cùng, trình biên dịch của Gleam biến đổi các chuỗi nội suy thành một loạt các ghép nối chuỗi trước khi nó được biên dịch sang Erlang, ngôn ngữ mà Gleam biên dịch sang. Vì vậy:

```gleam
"Hello, #{name}!"
```

Trở thành một cái gì đó giống như (trong pseudo-code Erlang):

```erlang
"Hello, " ++ name ++ "!"
```

Lựa chọn của nội suy so với ghép nối thường là về tính khả đọc và tiện lợi, mặc dù không có nhiều khác biệt về hiệu suất do tối ưu hóa bởi trình biên dịch.

## Xem thêm

- [Sách Gleam](https://gleam.run/book/)
- [Tài liệu mô-đun Chuỗi của Erlang](http://erlang.org/doc/man/string.html) để hiểu rõ hơn về cái mà Gleam được biên dịch xuống.
