---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:59.309568-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EA7u ti\xEAn, th\xEAm m\u1ED9t b\u1ED9\
  \ ph\xE2n t\xEDch TOML v\xE0o c\xE1c ph\u1EE5 thu\u1ED9c mix c\u1EE7a b\u1EA1n.\
  \ V\xED d\u1EE5 n\xE0y s\u1EED d\u1EE5ng `toml-elixir`."
lastmod: '2024-03-13T22:44:36.238434-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EA7u ti\xEAn, th\xEAm m\u1ED9t b\u1ED9 ph\xE2n t\xEDch TOML v\xE0\
  o c\xE1c ph\u1EE5 thu\u1ED9c mix c\u1EE7a b\u1EA1n."
title: "L\xE0m vi\u1EC7c v\u1EDBi TOML"
weight: 39
---

## Làm thế nào:
Đầu tiên, thêm một bộ phân tích TOML vào các phụ thuộc mix của bạn. Ví dụ này sử dụng `toml-elixir`:

```elixir
def deps do
  [
    {:toml_elixir, "~> 2.0"}
  ]
end
```

Đọc một tệp TOML:

```elixir
{:ok, toml_data} = File.read("config.toml")
{:ok, parsed_data} = TomlElixir.parse(toml_data)
```

Để chuyển đổi dữ liệu Elixir thành TOML:

```elixir
data = %{title: "Ví dụ TOML", owner: %{name: "Tom Preston-Werner"}}
toml_string = TomlElixir.encode(data)
```

Dữ liệu mẫu:

```elixir
"title = \"Ví dụ TOML\"\n\n[owner]\nname = \"Tom Preston-Werner\"\n"
```

## Khám phá sâu hơn
TOML được tạo ra bởi Tom Preston-Werner, đồng sáng lập của GitHub, để sử dụng trong các tệp cấu hình. Nó được thiết kế để dễ hiểu hơn XML và ngắn gọn hơn YAML trong khi vẫn giữ được sự nhất quán.

Các lựa chọn thay thế bao gồm các tệp JSON, YAML và INI, mỗi loại có những điểm đánh đổi trong đọc hiểu của con người và khả năng tương thích cấu trúc dữ liệu. TOML nổi bật trong việc biểu diễn rõ ràng dữ liệu bảng và nhóm lồng nhau của dữ liệu.

Trong Elixir, việc xử lý TOML phụ thuộc vào các thư viện mã hóa và giải mã, chuyển đổi chuỗi TOML thành các bản đồ Elixir và ngược lại. Việc phân tích hoạt động bằng cách khớp các quy tắc cú pháp của TOML và chuyển đổi chúng thành các loại dữ liệu của Elixir. Mã hóa thực hiện ngược lại bằng cách ánh xạ các loại dữ liệu của Elixir trở lại thành cú pháp TOML hợp lệ.

## Xem thêm
- Ngôn ngữ TOML: https://toml.io/en/
- Kho GitHub của `toml-elixir`: https://github.com/bitwalker/toml-elixir
- Chi tiết gói Hex cho `toml-elixir`: https://hex.pm/packages/toml_elixir
