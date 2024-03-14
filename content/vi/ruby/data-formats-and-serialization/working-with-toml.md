---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:24.374248-07:00
description: "TOML l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1ng t\u1EC7p c\u1EA5u h\xEC\
  nh d\u1EC5 \u0111\u1ECDc nh\u1EDD v\xE0o ng\u1EEF ngh\u0129a r\xF5 r\xE0ng c\u1EE7\
  a n\xF3. C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng TOML \u0111\u1EC3 qu\u1EA3\
  n l\xFD c\u1EA5u h\xECnh \u1EE9ng d\u1EE5ng v\xE0 chu\u1ED7i h\xF3a d\u1EEF\u2026"
lastmod: '2024-03-13T22:44:37.371788-06:00'
model: gpt-4-0125-preview
summary: "TOML l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1ng t\u1EC7p c\u1EA5u h\xECnh d\u1EC5\
  \ \u0111\u1ECDc nh\u1EDD v\xE0o ng\u1EEF ngh\u0129a r\xF5 r\xE0ng c\u1EE7a n\xF3\
  . C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng TOML \u0111\u1EC3 qu\u1EA3\
  n l\xFD c\u1EA5u h\xECnh \u1EE9ng d\u1EE5ng v\xE0 chu\u1ED7i h\xF3a d\u1EEF\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi TOML"
---

{{< edit_this_page >}}

## Gì & Tại sao?

TOML là một định dạng tệp cấu hình dễ đọc nhờ vào ngữ nghĩa rõ ràng của nó. Các lập trình viên sử dụng TOML để quản lý cấu hình ứng dụng và chuỗi hóa dữ liệu mà không cần đến sự nặng nề của XML hay những tính cách quái dị của YAML.

## Làm thế nào:

Đầu tiên, cài đặt gem `toml-rb`. Đây là một lựa chọn phổ biến cho việc phân tích TOML trong Ruby.

```Ruby
gem install toml-rb
```

Tiếp theo, đọc một tệp TOML:

```Ruby
require 'toml-rb'

toml_content = File.read('config.toml')
config = TomlRB.parse(toml_content)
puts config['title']
```

Một ví dụ về đầu ra có thể là:

```
My Awesome App
```

Ghi vào một tệp TOML:

```Ruby
require 'toml-rb'

config = {
  'title' => 'My Awesome App',
  'owner' => {
    'name' => 'John Doe',
    'dob' => Date.new(1979, 5, 27)
  }
}

toml_string = TomlRB.dump(config)
File.write('config.toml', toml_string)
```

Kiểm tra `config.toml` và bạn sẽ thấy cấu hình của mình, được lưu trữ gọn gàng.

## Sâu hơn

TOML, có nghĩa là Tom's Obvious, Minimal Language, được tạo ra bởi Tom Preston-Werner, đồng sáng lập của GitHub, vào khoảng năm 2013. Mục tiêu chính của nó là để trở thành một định dạng dễ hiểu, dễ phân tích thành các cấu trúc dữ liệu. Trong khi JSON tuyệt vời cho APIs, và YAML linh hoạt, sứ mệnh của TOML là nhấn mạnh vào việc thân thiện với con người. Không giống như YAML, có thể khá kén người với thụt lề, TOML hướng tới cấu trúc giống như INI, mà nhiều người thấy đơn giản và ít lỗi hơn.

Các lựa chọn khác như JSON, YAML, hoặc XML mỗi cái đều có ưu điểm của riêng mình, nhưng TOML tồn tại trong các tình huống mà cấu hình nên dễ dàng được bảo trì bởi con người và chương trình như nhau. Nó không chỉ đơn giản mà còn thực thi định dạng dễ đọc và chặt chẽ.

Về mặt kỹ thuật, để phân tích nội dung TOML với Ruby, chúng tôi tận dụng các gems như `toml-rb`. Gem này tận dụng bản chất động của Ruby, chuyển đổi dữ liệu TOML thành các hash, mảng và các cấu trúc dữ liệu cơ bản khác của Ruby. Sự chuyển đổi này có nghĩa là các nhà phát triển có thể làm việc với dữ liệu TOML sử dụng semantics và phương thức quen thuộc của Ruby.

## Xem thêm

- Dự án và spec TOML: https://toml.io/en/
- Gem `toml-rb`: https://github.com/emancu/toml-rb
- So sánh TOML, YAML, và JSON: https://blog.theodo.com/2021/08/compare-yml-toml-json/
