---
title:                "Tìm kiếm và thay thế văn bản"
date:                  2024-01-28T22:07:43.885298-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm kiếm và thay thế văn bản"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elixir/searching-and-replacing-text.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Tìm kiếm và thay thế văn bản là cơ bản của lập trình; chúng chủ yếu là tìm các chuỗi và thay thế chúng. Lập trình viên thực hiện việc này mọi lúc cho các công việc như cập nhật cơ sở mã, xử lý dữ liệu văn bản, hoặc chỉ đơn giản là các tác vụ chỉnh sửa.

## Làm thế nào:

Trong Elixir, bạn có thể sử dụng mô-đun `String` để thực hiện nhanh chóng các thao tác tìm và thay thế. Đây là cách bạn thực hiện:

```elixir
original_text = "I heart Elixir!"

# Thay thế đơn giản
replaced_text = String.replace(original_text, "heart", "❤️")
IO.puts replaced_text  # Đầu ra: I ❤️ Elixir!

# Thay thế toàn bộ với một mẫu
replaced_text_global = String.replace(original_text, ~r/eart|Eli/, "❤️", global: true)
IO.puts replaced_text_global  # Đầu ra: I ❤️ ❤️xir!

# Thay thế không phân biệt chữ hoa chữ thường
insensitive_replace = String.replace(original_text, "ELIXIR", "❤️", global: true, case_insensitive: true)
IO.puts insensitive_replace  # Đầu ra: I heart ❤️!
```

## Sâu hơn

Việc tìm kiếm và thay thế văn bản đã tồn tại từ thuở sơ khai của máy tính; nghĩ đến 'tìm và thay thế' trong một tài liệu Word, nhưng dành cho mã. Trong Elixir, nó liên quan đến việc phù hợp mẫu và làm việc hiệu quả với chuỗi. 

Hàm `String.replace/4` tận dụng khả năng phù hợp mẫu của Elixir, cho phép bạn không chỉ phù hợp với các chuỗi tĩnh mà còn cả các mẫu regex, cung cấp sự linh hoạt đáng kể. Đằng sau hậu trường, Elixir sử dụng khả năng xử lý chuỗi mạnh mẽ của Erlang, đó là mạnh mẽ và hiệu quả cho các tác vụ xử lý văn bản.

Các lựa chọn thay thế cho mô-đun `String` built-in bao gồm viết các hàm của riêng bạn cho các trường hợp phức tạp hơn hoặc sử dụng các thư viện bên thứ ba bao bọc xử lý chuỗi theo các cách khác nhau. Tuy nhiên, đối với hầu hết các trường hợp sử dụng, các hàm built-in sẽ hoàn thành công việc mà không cần thêm các phụ thuộc bên ngoài.

Với tư cách là một ngôn ngữ bất biến, hãy nhớ rằng mỗi hàm thay thế trả về một chuỗi mới - chuỗi gốc không thay đổi. Điều này khác với một số ngôn ngữ khác khi bạn có thể thay đổi chuỗi tại chỗ.

## Xem thêm

- Tài liệu mô-đun `String` của Elixir: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- Regex trong Elixir: [https://hexdocs.pm/elixir/Regex.html](https://hexdocs.pm/elixir/Regex.html)
- Tìm hiểu thêm về phù hợp mẫu trong Elixir: [https://elixir-lang.org/getting-started/pattern-matching.html](https://elixir-lang.org/getting-started/pattern-matching.html)
