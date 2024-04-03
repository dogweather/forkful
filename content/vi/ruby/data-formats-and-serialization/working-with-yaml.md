---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:03.701063-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi YAML\
  \ trong Ruby, b\u1EA1n c\u1EA7n th\u01B0 vi\u1EC7n `yaml`. N\xF3 l\xE0 m\u1ED9t\
  \ ph\u1EA7n c\u1EE7a th\u01B0 vi\u1EC7n chu\u1EA9n c\u1EE7a Ruby, v\xEC v\u1EAD\
  y ch\u1EC9 c\u1EA7n y\xEAu c\u1EA7u n\xF3."
lastmod: '2024-03-13T22:44:37.368058-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 l\xE0m vi\u1EC7c v\u1EDBi YAML trong Ruby, b\u1EA1n c\u1EA7\
  n th\u01B0 vi\u1EC7n `yaml`."
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
weight: 41
---

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
