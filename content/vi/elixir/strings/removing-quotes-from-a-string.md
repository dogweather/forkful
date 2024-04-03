---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:15.305111-07:00
description: "L\xE0m th\u1EBF n\xE0o: Elixir kh\xF4ng c\xF3 h\xE0m 'lo\u1EA1i b\u1ECF\
  \ d\u1EA5u ngo\u1EB7c' s\u1EB5n c\xF3, nh\u01B0ng r\u1EA5t d\u1EC5 \u0111\u1EC3\
  \ t\u1EF1 t\u1EA1o m\u1ED9t c\xE1i v\u1EDBi vi\u1EC7c s\u1EED d\u1EE5ng k\u1EF9\
  \ thu\u1EADt kh\u1EDBp m\u1EABu ho\u1EB7c c\xE1c h\xE0m `String`. Xem\u2026"
lastmod: '2024-03-13T22:44:36.192640-06:00'
model: gpt-4-0125-preview
summary: "Elixir kh\xF4ng c\xF3 h\xE0m 'lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c' s\u1EB5\
  n c\xF3, nh\u01B0ng r\u1EA5t d\u1EC5 \u0111\u1EC3 t\u1EF1 t\u1EA1o m\u1ED9t c\xE1\
  i v\u1EDBi vi\u1EC7c s\u1EED d\u1EE5ng k\u1EF9 thu\u1EADt kh\u1EDBp m\u1EABu ho\u1EB7\
  c c\xE1c h\xE0m `String`."
title: "Lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c k\xE9p kh\u1ECFi chu\u1ED7i"
weight: 9
---

## Làm thế nào:
Elixir không có hàm 'loại bỏ dấu ngoặc' sẵn có, nhưng rất dễ để tự tạo một cái với việc sử dụng kỹ thuật khớp mẫu hoặc các hàm `String`. Xem các đoạn mã sau:

```elixir
# Sử dụng khớp mẫu
def unquote_string("\"" <> quoted_string <> "\""), do: quoted_string
def unquote_string("'" <> quoted_string <> "'"), do: quoted_string
def unquote_string(quoted_string), do: quoted_string

# Ví dụ sử dụng
unquote_string("\"Hello, World!\"") # => "Hello, World!"
unquote_string("'Hello, World!'")   # => "Hello, World!"

# Sử dụng String.trim/1
def unquote_string(string), do: String.trim(string, "'\"")

# Ví dụ sử dụng
unquote_string("\"Hello, World!\"") # => "Hello, World!"
unquote_string("'Hello, World!'")   # => "Hello, World!"
```

Kết quả cho cả hai phương pháp sẽ là:
```
"Hello, World!"
```

## Sâu hơn
Ngày xưa, dấu ngoặc trong các chuỗi là một mỏ mìn—xử lý không cẩn thận, và bum, lỗi cú pháp hoặc lỗ hổng bảo mật. Trong Elixir, khớp mẫu xử lý các chuỗi của bạn như các khối Lego, cho phép bạn tháo rời và tái lắp với độ chính xác. Mô-đun `String` mạnh mẽ của nó cũng rất hữu ích, linh hoạt loại bỏ dấu ngoặc với các hàm `trim`. Các phương án khác? Biểu thức chính quy có thể đá dấu ngoặc ra khỏi ván, và các thư viện bên ngoài có thể cung cấp thêm sức mạnh nếu bạn cần nhiều hơn việc loại bỏ cơ bản.

## Xem thêm
Tìm hiểu sâu hơn với những tài liệu này:
- [Mô-đun String của Elixir](https://hexdocs.pm/elixir/String.html)
- [Tìm hiểu thêm về khớp mẫu trong Elixir](https://elixir-lang.org/getting-started/pattern-matching.html)
- [Biểu thức chính quy trong Elixir (Mô-đun Regex)](https://hexdocs.pm/elixir/Regex.html)
