---
title:                "Chuyển đổi chuỗi thành chữ thường"
date:                  2024-01-28T21:58:28.223225-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi chuỗi thành chữ thường"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Trong Ruby, việc chuyển đổi một chuỗi thành chữ thường có nghĩa là thay đổi tất cả các chữ cái viết hoa trong chuỗi thành các chữ cái viết thường tương ứng. Lập trình viên làm điều này để đảm bảo tính nhất quán, đặc biệt là trong các nhiệm vụ như so sánh đầu vào từ người dùng hoặc sắp xếp.

## Làm thế nào:

```ruby
# Sử dụng phương thức downcase
my_string = "Hello World!"
puts my_string.downcase  # => "hello world!"
```

```ruby
# Sử dụng downcase! cho biến đổi tại chỗ
my_string = "Hello World!"
my_string.downcase!
puts my_string           # => "hello world!"
```

## Sâu hơn

Theo lịch sử, việc chuyển đổi kiểu chữ đã trở thành một điều cơ bản trong các ngôn ngữ lập trình để đảm bảo tính đồng nhất của văn bản. Nó hỗ trợ so sánh và tìm kiếm không phân biệt chữ hoa chữ thường, do đó quan trọng của nó.

Các phương thức `downcase` và `downcase!` trong Ruby xuất phát từ nguyên tắc của ngôn ngữ này là cung cấp cả phương thức không phá hủy và phá hủy cho việc thao tác chuỗi. Phương thức không phá hủy `downcase` trả về một chuỗi mới, để nguyên bản không đổi, trong khi `downcase!` phá hủy sửa đổi chuỗi gốc tại chỗ, có thể hiệu quả hơn về mặt bộ nhớ.

Có những phương pháp thay thế cho các trường hợp khi các quy tắc cụ thể của địa phương được áp dụng. `String#mb_chars` kết hợp với `ActiveSupport::Multibyte::Chars#downcase` từ thư viện ActiveSupport của Rails có thể xử lý các tình huống phức tạp hơn như các ký tự có dấu hoặc các dấu diacritical khác:

```ruby
require 'active_support/core_ext/string/multibyte'

my_string = "ÄÖÜ"
puts my_string.mb_chars.downcase  # => "äöü"
```

Về việc triển khai, `downcase` và `downcase!` của Ruby nội bộ sử dụng bản đồ Unicode để chuyển đổi từng ký tự của chuỗi thành bản tương đương chữ thường của nó.

## Xem Thêm

- Tài liệu Ruby cho `downcase` và `downcase!`: [Ruby Doc downcase](https://ruby-doc.org/core-3.1.2/String.html#method-i-downcase), [Ruby Doc downcase!](https://ruby-doc.org/core-3.1.2/String.html#method-i-downcase-21)
- Đối với các chuyển đổi trường hợp phức tạp, xem các mở rộng cốt lõi ActiveSupport: [ActiveSupport String](https://api.rubyonrails.org/classes/String.html)
