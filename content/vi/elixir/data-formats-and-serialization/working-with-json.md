---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:34.982147-07:00
description: "C\xE1ch l\xE0m: \u0110\u1EC3 x\u1EED l\xFD JSON trong Elixir, ch\xFA\
  ng ta s\u1EED d\u1EE5ng c\xE1c th\u01B0 vi\u1EC7n nh\u01B0 `Jason` ho\u1EB7c `Poison`.\
  \ D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t h\u01B0\u1EDBng d\u1EABn nhanh v\u1EDB\
  i `Jason`."
lastmod: '2024-03-13T22:44:36.236011-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 x\u1EED l\xFD JSON trong Elixir, ch\xFAng ta s\u1EED d\u1EE5\
  ng c\xE1c th\u01B0 vi\u1EC7n nh\u01B0 `Jason` ho\u1EB7c `Poison`."
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
weight: 38
---

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
