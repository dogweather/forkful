---
title:                "Sử dụng biểu thức chính quy"
date:                  2024-01-28T22:09:45.733411-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng biểu thức chính quy"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elixir/using-regular-expressions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Biểu thức chính quy (regex) là các mẫu được sử dụng để khớp các kết hợp ký tự trong văn bản. Các lập trình viên sử dụng chúng cho các nhiệm vụ như xác nhận định dạng, tìm kiếm và thay thế văn bản, và phân tích dữ liệu từ các chuỗi phức tạp.

## Làm thế nào:

Trong Elixir, bạn sử dụng regex với các mẫu có sẵn hoặc tạo ra chúng của riêng bạn với module `Regex`. Dưới đây là một ví dụ nhanh:

```elixir
# Tìm kiếm từ "hello"
regex = ~r/hello/
"hello world" =~ regex
# => true

# Tìm kiếm không phân biệt chữ hoa chữ thường
regex = ~r/hello/i
"Hello world" =~ regex
# => true

# Thay thế "world" bằng "Elixir"
"hello world" |> String.replace(~r/world/, "Elixir")
# => "hello Elixir"
```

## Đào Sâu Hơn

Regex được tiên phong vào những năm 1950 bởi nhà toán học Stephen Kleene. Elixir triển khai regex thông qua thư viện PCRE (Perl Compatible Regular Expressions), có khả năng khớp mẫu một cách mạnh mẽ. Các phương án thay thế như khớp chuỗi với `String.contains?/2` hay `String.starts_with?/2` tồn tại, nhưng chúng thiếu đi sự linh hoạt mà regex cung cấp. Module `Regex` của Elixir biên dịch các mẫu thành một định dạng nội bộ được tối ưu hóa cho việc sử dụng lặp đi lặp lại, tiết kiệm thời gian tính toán.

## Xem Thêm

- Tài liệu module `Regex` của Elixir: [https://hexdocs.pm/elixir/Regex.html](https://hexdocs.pm/elixir/Regex.html)
- Regex101, một công cụ kiểm tra và gỡ lỗi regex trực tuyến: [https://regex101.com/](https://regex101.com/)
- "Programming Elixir" của Dave Thomas - một hướng dẫn toàn diện cũng bao gồm việc sử dụng regex.
