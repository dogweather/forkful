---
title:                "Chuyển đổi chuỗi thành chữ thường"
date:                  2024-01-28T21:58:18.818462-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi chuỗi thành chữ thường"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Chuyển đổi một chuỗi thành chữ thường có nghĩa là biến tất cả các ký tự viết hoa thành các ký tự viết thường tương ứng. Lập trình viên thực hiện điều này để đảm bảo tính nhất quán, đặc biệt là cho các phép so sánh hoặc để chuẩn hóa dữ liệu nhập của người dùng.

## Cách thực hiện:
Trong Gleam, việc thao tác với chuỗi là khá đơn giản. Sử dụng hàm `string.lowercase` để chuyển một chuỗi thành chữ thường. Dưới đây là một ví dụ đơn giản:

```gleam
import gleam/string

pub fn demo() {
  let my_string = "Hello, World!"
  string.lowercase(my_string)
}

// Kết quả sẽ là: "hello, world!"
```

## Đào sâu
Trước khi Unicode trở nên phổ biến, chúng ta có ASCII, nơi chuyển đổi thành chữ thường chỉ là một chút toán học. Cộng 32 vào mã ký tự ASCII viết hoa là sẽ có ký tự viết thường. Với Unicode, nó phức tạp hơn; các quy tắc phức tạp và đặc thù theo ngôn ngữ. Trong Gleam, tất cả các công việc nặng nhọc đã được ẩn đi, mang đến cho bạn một hàm đơn giản hoạt động một cách nhất quán qua các ngôn ngữ khác nhau.

Các phương pháp thay thế cho string.lowercase có thể bao gồm việc lập bản đồ thủ công từng ký tự trong một chuỗi, kiểm tra xem nó có phải là chữ viết hoa hay không và sau đó chuyển đổi nó. Điều này khả thi nhưng tại sao lại phải tái phát minh cái bánh xe?

Bên trong, một hàm như `string.lowercase` có thể dựa vào thư viện unicode của hệ thống Erlang đang nền tảng, bao gồm hỗ trợ mạnh mẽ cho việc ánh xạ chữ hoa và chữ thường trong các ngôn ngữ khác nhau. Đây là một chủ đề cực kỳ phức tạp bởi vì ý tưởng về chữ hoa và chữ thường thậm chí không áp dụng cho một số lượng lớn hệ thống viết.

## Xem thêm
- Câu hỏi thường gặp về Ánh xạ Chữ hoa trong Unicode: [http://www.unicode.org/faq/casemap_charprop.html](http://www.unicode.org/faq/casemap_charprop.html)
- Mô đun unicode của Erlang để nhìn nhận bên trong: [https://www.erlang.org/doc/man/unicode.html](https://www.erlang.org/doc/man/unicode.html)
