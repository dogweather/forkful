---
title:                "Làm việc với YAML"
aliases:
- vi/ruby/working-with-yaml.md
date:                  2024-01-28T22:12:03.701063-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/working-with-yaml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
YAML là viết tắt của "YAML Ain't Markup Language" (YAML không phải là Ngôn ngữ Đánh dấu). Đây là một định dạng tuần tự hóa dữ liệu dễ đọc cho con người. Các lập trình viên sử dụng nó cho các tệp cấu hình, trao đổi dữ liệu giữa các ngôn ngữ, và bởi vì nó dễ đọc hơn JSON hoặc XML đối với các cấu trúc dữ liệu phức tạp.

## Làm thế nào:

Để làm việc với YAML trong Ruby, bạn cần thư viện `yaml`. Nó là một phần của thư viện chuẩn của Ruby, vì vậy chỉ cần yêu cầu nó:

```ruby
require 'yaml'
```

Để đổ một hash Ruby vào một chuỗi YAML:

```ruby
require 'yaml'

my_hash = { name: 'Sam', occupation: 'Developer', hobbies: ['coding', 'chess'] }

yaml_string = my_hash.to_yaml
puts yaml_string
```

Kết quả sẽ là một chuỗi định dạng YAML:

```
---
:name: Sam
:occupation: Developer
:hobbies:
- coding
- chess
```

Để tải một chuỗi YAML vào Ruby:

```ruby
require 'yaml'

yaml_string = "
name: Sam
occupation: Developer
hobbies:
  - coding
  - chess
"

ruby_hash = YAML.load(yaml_string)
puts ruby_hash
```

Kết quả là một hash Ruby:

```
{name: 'Sam', occupation: 'Developer', hobbies: ['coding', 'chess']}
```

## Tìm hiểu sâu

YAML xuất hiện vào đầu những năm 2000 như một lựa chọn thân thiện với con người thay thế XML cho các tệp cấu hình và tuần tự hóa dữ liệu. Thiết kế của nó cho phép ánh xạ dễ dàng đến các cấu trúc dữ liệu gốc trong nhiều ngôn ngữ, có các thực hiện trong Python, Ruby, Java, PHP, và các ngôn ngữ khác.

Các lựa chọn thay thế cho YAML bao gồm JSON và TOML. JSON phổ biến hơn cho các API web do tương thích trực tiếp với JavaScript. TOML nhằm mục đích dễ đọc hơn như một tệp cấu hình trong khi cung cấp một bộ tính năng tương tự như YAML.

Trong Ruby, YAML được thực hiện bởi thư viện Psych, đã trở thành trình phân tích cú pháp YAML mặc định kể từ Ruby 1.9.3. Psych tương tác với libyaml, một thư viện C cho phân tích cú pháp và phát sinh YAML.

## Xem thêm

- [Trang Chính thức của YAML](https://yaml.org/)
- [Tài liệu Thư viện Psych](https://ruby-doc.org/stdlib-3.0.0/libdoc/psych/rdoc/Psych.html)
- [Tài liệu Mô đun YAML của Ruby](https://ruby-doc.org/stdlib-2.5.1/libdoc/yaml/rdoc/YAML.html)
- [Trang Chính thức của JSON (JavaScript Object Notation)](https://www.json.org/)
- [Kho GitHub của TOML (Tom's Obvious, Minimal Language)](https://github.com/toml-lang/toml)
