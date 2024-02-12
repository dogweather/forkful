---
title:                "Làm việc với YAML"
date:                  2024-01-28T22:11:44.337175-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/elixir/working-with-yaml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
