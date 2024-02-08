---
title:                "Xóa các ký tự phù hợp với một mẫu"
aliases:
- vi/elixir/deleting-characters-matching-a-pattern.md
date:                  2024-01-28T21:59:14.304736-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xóa các ký tự phù hợp với một mẫu"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elixir/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Xóa ký tự phù hợp với một mẫu là việc tìm kiếm các chuỗi ký tự cụ thể và loại bỏ chúng. Lập trình viên làm điều này để làm sạch dữ liệu, định dạng nội dung, hoặc thao tác chuỗi theo cách phù hợp với nhu cầu cụ thể của họ.

## Cách làm:

Trong Elixir, sử dụng hàm `String.replace/4` để xóa ký tự phù hợp với một mẫu. Hãy xem những ví dụ sau:

```elixir
# Xóa số từ một chuỗi
original_string = "Elixir2023Rocks!"
clean_string = String.replace(original_string, ~r/\d/, "")
IO.puts(clean_string) # Kết quả: "ElixirRocks!"

# Gỡ bỏ dấu câu
punctuationless_string = String.replace(original_string, ~r/[[:punct:]]/, "")
IO.puts(punctuationless_string) # Kết quả: "Elixir2023Rocks"

# Làm sạch khoảng trắng
no_whitespace_string = String.replace(original_string, ~r/\s/, "")
IO.puts(no_whitespace_string) # Kết quả: "Elixir2023Rocks!"
```

## Tìm hiểu sâu hơn

Việc sử dụng tương thích mẫu để xóa ký tự trong chuỗi không phải là độc quyền của Elixir; nó là một tính năng phổ biến trong hầu hết các ngôn ngữ lập trình, phát triển từ khả năng thể hiện chính quy (regex) trong các công cụ Unix đầu tiên như `sed` và `grep`. Các phương pháp thay thế cho `String.replace/4` có thể là sử dụng tương thích mẫu và đệ quy để thực hiện điều hướng và chỉnh sửa chuỗi một cách thủ công, nhưng phương pháp này thường dài dòng và phức tạp hơn, làm cho các hàm regex tích hợp trở thành lựa chọn ưu tiên. Bên dưới cùng, `String.replace/4` tận dụng di sản Erlang của Elixir, sử dụng khả năng tương thích mẫu mạnh mẽ và thao tác chuỗi của máy ảo BEAM.

## Xem thêm:

- Tài liệu mô-đun `String` của Elixir: [https://hexdocs.pm/elixir/String.html](https://hexdocs.pm/elixir/String.html)
- Regex trong Elixir: [https://hexdocs.pm/elixir/Regex.html](https://hexdocs.pm/elixir/Regex.html)
- 'Học Biểu thức Chính quy': [https://www.regular-expressions.info/tutorial.html](https://www.regular-expressions.info/tutorial.html)
- Cách tiếp cận chuỗi và tương thích mẫu của trường học Elixir: [https://elixirschool.com/en/lessons/basics/strings/](https://elixirschool.com/en/lessons/basics/strings/)
