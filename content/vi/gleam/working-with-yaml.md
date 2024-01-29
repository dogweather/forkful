---
title:                "Làm việc với YAML"
date:                  2024-01-28T22:12:35.727838-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với YAML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/gleam/working-with-yaml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
YAML, viết tắt của "YAML Ain't Markup Language", là một chuẩn hóa dữ liệu có thể đọc được bởi con người. Lập trình viên sử dụng nó cho các tệp cấu hình, trao đổi dữ liệu giữa các ngôn ngữ, và bởi vì nó dễ đọc hơn JSON hoặc XML.

## Làm thế nào:
Hiện tại, Gleam không có bộ phân tích cú pháp YAML hoặc thư viện được tích hợp sẵn, tính đến lần cập nhật cuối cùng của tôi. Bạn thường sẽ phân tích cú pháp YAML trong Gleam bằng cách dựa vào các hàm của Erlang nhờ vào khả năng tương thích của Gleam với hệ sinh thái của Erlang. Hãy sử dụng một thư viện Erlang và gọi nó từ Gleam.

Đầu tiên, thêm thư viện YAML của Erlang vào `rebar.config`:

```erlang
{deps, [yaml]}.
```

Dưới đây là cách bạn có thể gọi thư viện Erlang từ Gleam:

```rust
external fn parse(String) -> Result(Tuple(tuple(atom(), String)), Nil) =
  "yaml" "decode"

pub fn main() -> Result(Tuple(tuple(atom(), String)), Nil) {
  let yaml_data = "greeting: hello"
  parse(yaml_data)
}
```

Kết quả mẫu có thể trông như thế này:

```elixir
Ok(#(ok, [{greeting, "hello"}]))
```

## Kỹ thuật Sâu hơn
YAML được phát hành vào năm 2001 và thường được sử dụng ở những nơi cần độ dễ đọc bởi con người. Nó không phải lúc nào cũng là mặc định cho việc chuẩn hóa dữ liệu, JSON và XML cũng được sử dụng rộng rãi. Tuy nhiên, sự đơn giản của YAML làm cho nó lý tưởng cho các tệp cấu hình hoặc cấu trúc dữ liệu đơn giản.

Các phương án thay thế có thể là bộ phân tích cú pháp `:yamerl` được tích hợp sẵn trong Elixir, và trong Gleam, bạn có thể xử lý các nhiệm vụ tương tự bằng cách sử dụng JSON với thư viện `gleam/json`. Về việc triển khai, bạn đang tận dụng hệ sinh thái BEAM rộng lớn khi bạn làm việc với YAML trong Gleam—đó là tính tương thích này làm cho việc phân tích cú pháp YAML trở nên khả thi mà không cần thư viện Gleam dédicated.

## Xem thêm
- Đặc tả YAML: https://yaml.org/spec/1.2/spec.html
- Thư viện `yaml` của Erlang: https://hex.pm/packages/yaml
- Tài liệu thư viện JSON của Gleam: https://hexdocs.pm/gleam_stdlib/gleam/json/
