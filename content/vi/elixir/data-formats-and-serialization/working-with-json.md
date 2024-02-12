---
title:                "Làm việc với JSON"
aliases:
- vi/elixir/working-with-json.md
date:                  2024-01-28T22:10:34.982147-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elixir/working-with-json.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
JSON (JavaScript Object Notation) là một định dạng trao đổi dữ liệu nhẹ, dễ đọc và viết cho con người cũng như dễ dàng phân tích và tạo ra bởi máy móc. Lập trình viên làm việc với JSON để trao đổi dữ liệu giữa các máy chủ và ứng dụng web, lưu trữ cấu hình hoặc tuần tự hóa dữ liệu cho giao tiếp mạng.

## Cách làm:

Để xử lý JSON trong Elixir, chúng ta sử dụng các thư viện như `Jason` hoặc `Poison`. Dưới đây là một hướng dẫn nhanh với `Jason`:

```elixir
# Thêm Jason vào mix.exs của bạn như một sự phụ thuộc
{:jason, "~> 1.3"}

# trong một file .ex, để mã hóa Elixir sang JSON
json_string = Jason.encode!(%{foo: "bar"})

# Bây giờ giải mã JSON sang Elixir
elixir_map = Jason.decode!(json_string)
```

Kết quả:

```elixir
json_string #=> "{\"foo\":\"bar\"}"
elixir_map  #=> %{"foo" => "bar"}
```

Mã hóa với `opts` cho việc in đẹp:

```elixir
Jason.encode!(%{foo: "bar"}, pretty: true)
```

Kết quả:

```json
{
  "foo": "bar"
}
```

## Thâm nhập

JSON được đề xuất bởi Douglas Crockford vào đầu những năm 2000. Nó nhanh chóng được chấp nhận do sự đơn giản so với XML.

Có sự thay thế? Chắc chắn - XML, YAML, hoặc Protocol Buffers, tuy nhiên JSON vẫn thống trị do sự đơn giản và hỗ trợ bản địa bởi JavaScript.

Bên dưới lớp vỏ, các thư viện JSON chuyển đổi giữa các kiểu dữ liệu Elixir sang chuỗi JSON và ngược lại. Khớp mẫu và thư viện tiêu chuẩn mạnh mẽ của Elixir làm cho quá trình mã hóa và giải mã trở nên trôi chảy.

## Xem thêm

- GitHub của Jason: https://github.com/michalmuskala/jason
- GitHub của Poison: https://github.com/devinus/poison
- Bài học về JSON tại Elixir School: https://elixirschool.com/en/lessons/specifics/jason/
