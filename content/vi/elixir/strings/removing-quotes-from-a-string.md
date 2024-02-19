---
aliases:
- /vi/elixir/removing-quotes-from-a-string/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:15.305111-07:00
description: "Lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c kh\u1ECFi m\u1ED9t chu\u1ED7i\
  \ c\xF3 ngh\u0129a l\xE0 b\u1ECF \u0111i nh\u1EEFng bao b\u1ECDc th\u1EEBa \u0111\
  \u1EC3 l\u1EA5y \u0111\u01B0\u1EE3c v\u0103n b\u1EA3n s\u1EA1ch b\xEAn trong. L\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 l\xE0m s\u1EA1ch d\u1EEF\
  \ li\u1EC7u\u2026"
lastmod: 2024-02-18 23:08:50.349554
model: gpt-4-0125-preview
summary: "Lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c kh\u1ECFi m\u1ED9t chu\u1ED7i c\xF3\
  \ ngh\u0129a l\xE0 b\u1ECF \u0111i nh\u1EEFng bao b\u1ECDc th\u1EEBa \u0111\u1EC3\
  \ l\u1EA5y \u0111\u01B0\u1EE3c v\u0103n b\u1EA3n s\u1EA1ch b\xEAn trong. L\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 l\xE0m s\u1EA1ch d\u1EEF\
  \ li\u1EC7u\u2026"
title: "Lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c k\xE9p kh\u1ECFi chu\u1ED7i"
---

{{< edit_this_page >}}

## Gì và Tại sao?
Loại bỏ dấu ngoặc khỏi một chuỗi có nghĩa là bỏ đi những bao bọc thừa để lấy được văn bản sạch bên trong. Lập trình viên làm điều này để làm sạch dữ liệu đầu vào, tránh lỗi, và chuẩn bị dữ liệu cho việc xử lý nơi mà dấu ngoặc là trở ngại, không phải tính năng.

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
