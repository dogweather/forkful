---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:44.337175-07:00
description: "L\xE0m vi\u1EC7c v\u1EDBi YAML c\xF3 ngh\u0129a l\xE0 ph\xE2n t\xED\
  ch v\xE0 t\u1EA1o ra d\u1EEF li\u1EC7u \u0111\u1ECBnh d\u1EA1ng YAML, m\u1ED9t chu\u1EA9\
  n h\xF3a d\u1EEF li\u1EC7u \u0111\u1ECDc \u0111\u01B0\u1EE3c cho con ng\u01B0\u1EDD\
  i. L\u1EADp tr\xECnh vi\xEAn l\xE0m vi\u1EC7c n\xE0y cho\u2026"
lastmod: '2024-03-13T22:44:36.234793-06:00'
model: gpt-4-0125-preview
summary: "L\xE0m vi\u1EC7c v\u1EDBi YAML c\xF3 ngh\u0129a l\xE0 ph\xE2n t\xEDch v\xE0\
  \ t\u1EA1o ra d\u1EEF li\u1EC7u \u0111\u1ECBnh d\u1EA1ng YAML, m\u1ED9t chu\u1EA9\
  n h\xF3a d\u1EEF li\u1EC7u \u0111\u1ECDc \u0111\u01B0\u1EE3c cho con ng\u01B0\u1EDD\
  i. L\u1EADp tr\xECnh vi\xEAn l\xE0m vi\u1EC7c n\xE0y cho\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
---

{{< edit_this_page >}}

## Gì & Tại sao?

Làm việc với YAML có nghĩa là phân tích và tạo ra dữ liệu định dạng YAML, một chuẩn hóa dữ liệu đọc được cho con người. Lập trình viên làm việc này cho các tệp cấu hình, trao đổi dữ liệu, và vì nó dễ đọc hơn JSON hay XML cho các cấu trúc dữ liệu phức tạp.

## Làm thế nào:

Elixir không bao gồm hỗ trợ YAML tích hợp sẵn, nhưng bạn có thể sử dụng thư viện `yamerl`. Đầu tiên, thêm `yamerl` vào tệp `mix.exs` của bạn:

```elixir
defp deps do
  [{:yamerl, "~> 0.8"}]
end
```

Sau khi chạy `mix deps.get`, bạn có thể phân tích YAML:

```elixir
yml_data = """
name: John Doe
age: 30
langs:
  - Elixir
  - Ruby
  - Haskell
"""

parsed_data = :yamerl_constr.string(yml_data) |> Enum.take(1)
IO.inspect(parsed_data)
```

Điều này sẽ xuất ra:

```elixir
[
  %{
    "age" => 30,
    "langs" => ["Elixir", "Ruby", "Haskell"],
    "name" => "John Doe"
  }
]
```

Và để chuyển đổi dữ liệu Elixir sang YAML:

```elixir
data = %{
  name: "John Doe",
  age: 30,
  langs: ["Elixir", "Ruby", "Haskell"]
}

yml_string = :yamerl.encode(data)
IO.puts yml_string
```

Điều này in ra:

```yaml
---
age: 30
langs:
  - Elixir
  - Ruby
  - Haskell
name: John Doe
```

## Sâu hơn

YAML, viết tắt của "YAML Ain't Markup Language" (một từ viết tắt đệ quy), đã có từ năm 2001. JSON và XML có thể phục vụ mục đích tương tự nhưng sự tập trung vào tính dễ đọc của YAML khiến nó trở nên phổ biến cho cấu hình. `yamerl`, một thư viện Erlang được điều chỉnh cho Elixir thông qua tính tương thích, là một lựa chọn vững chắc cho các lập trình viên Elixir. Nhớ lại, YAML rất nhạy cảm với thụt lề, khiến việc phân tích trở nên phức tạp hơn so với JSON.

## Xem thêm

- Kho GitHub chính thức `yamerl`: https://github.com/yakaz/yamerl
- Elixir `hexdocs` cho các thư viện YAML: https://hex.pm/packages?search=yaml&sort=recent_downloads
- Trang web chính thức của YAML cho thông số kỹ thuật và hơn thế nữa: https://yaml.org
- Elixir School cho việc học Elixir: https://elixirschool.com/en/
