---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:46.116839-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Elixir, b\u1EA1n c\xF3 th\u1EC3 ph\xE2\
  n t\xEDch c\xFA ph\xE1p HTML v\u1EDBi th\u01B0 vi\u1EC7n Floki. D\u01B0\u1EDBi \u0111\
  \xE2y l\xE0 m\u1ED9t \u0111o\u1EA1n m\xE3."
lastmod: '2024-03-13T22:44:36.205436-06:00'
model: gpt-4-0125-preview
summary: "Trong Elixir, b\u1EA1n c\xF3 th\u1EC3 ph\xE2n t\xEDch c\xFA ph\xE1p HTML\
  \ v\u1EDBi th\u01B0 vi\u1EC7n Floki."
title: "Ph\xE2n T\xEDch C\xFA Ph\xE1p HTML"
weight: 43
---

## Làm thế nào:
Trong Elixir, bạn có thể phân tích cú pháp HTML với thư viện Floki. Dưới đây là một đoạn mã:

```elixir
# Đầu tiên, thêm Floki vào các phụ thuộc của mix.exs của bạn
{:floki, "~> 0.30.0"}

# Sau đó, trong mã của bạn

defmodule HTMLParser do
  alias Floki

  def parse_html(html) do
    {:ok, document} = Floki.parse(html)
    titles = Floki.find(document, "h1")
    IO.inspect(titles, label: "Tiêu đề")
  end
end

# Cách sử dụng
html_content = "<html><body><h1>Xin chào, Elixir!</h1></body></html>"
HTMLParser.parse_html(html_content)

# Kết quả mẫu
Tiêu đề: [{"h1", [], ["Xin chào, Elixir!"]}]
```

## Đi Sâu Hơn
Trong lịch sử, việc phân tích cú pháp HTML trong các ngôn ngữ như Python hoặc JavaScript đã phổ biến hơn, nhưng các tính năng đồng thời và khả năng mở rộng của Elixir làm cho nó trở thành một lựa chọn mạnh mẽ cho các tác vụ web hiện đại. Thư viện Floki sử dụng trình phân tích cú pháp C fast_html ở phía dưới để tăng tốc độ, mang lại cho bạn cả hai thế giới tốt nhất: đồng thời của Elixir và hiệu suất của một ngôn ngữ được biên dịch.

So với các công cụ khác như BeautifulSoup trong Python, Floki ít dài dòng hơn và mang phong cách chức năng hơn - phù hợp với tinh thần của Elixir. Hơn nữa, bạn có toàn bộ sức mạnh của hệ sinh thái Erlang cho khả năng chống lỗi và phân phối, nếu bạn đang nghĩ lớn.

## Xem thêm
- [Floki trên Hex](https://hex.pm/packages/floki) - Tài liệu chính thức của Floki.
- [HTML5ever](https://github.com/servo/html5ever) - Bộ phân tích cú pháp HTML của Rust hỗ trợ fast_html.
