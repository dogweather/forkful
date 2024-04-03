---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:28.223225-07:00
description: "L\xE0m th\u1EBF n\xE0o: ."
lastmod: '2024-03-13T22:44:37.315018-06:00'
model: gpt-4-0125-preview
summary: .
title: "Chuy\u1EC3n \u0111\u1ED5i chu\u1ED7i th\xE0nh ch\u1EEF th\u01B0\u1EDDng"
weight: 4
---

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
