---
title:                "Loại bỏ dấu ngoặc kép khỏi chuỗi"
date:                  2024-01-28T22:06:15.305111-07:00
model:                 gpt-4-0125-preview
simple_title:         "Loại bỏ dấu ngoặc kép khỏi chuỗi"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elixir/removing-quotes-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
