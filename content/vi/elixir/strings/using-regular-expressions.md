---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:45.733411-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Elixir, b\u1EA1n s\u1EED d\u1EE5ng regex\
  \ v\u1EDBi c\xE1c m\u1EABu c\xF3 s\u1EB5n ho\u1EB7c t\u1EA1o ra ch\xFAng c\u1EE7\
  a ri\xEAng b\u1EA1n v\u1EDBi module `Regex`. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9\
  t v\xED d\u1EE5 nhanh."
lastmod: '2024-03-13T22:44:36.195191-06:00'
model: gpt-4-0125-preview
summary: "Trong Elixir, b\u1EA1n s\u1EED d\u1EE5ng regex v\u1EDBi c\xE1c m\u1EABu\
  \ c\xF3 s\u1EB5n ho\u1EB7c t\u1EA1o ra ch\xFAng c\u1EE7a ri\xEAng b\u1EA1n v\u1EDB\
  i module `Regex`."
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
weight: 11
---

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
