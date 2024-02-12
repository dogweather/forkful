---
title:                "Sử dụng biểu thức chính quy"
aliases: - /vi/ruby/using-regular-expressions.md
date:                  2024-01-28T22:09:58.843349-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sử dụng biểu thức chính quy"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/using-regular-expressions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
Biểu thức chính quy (regex) là các mẫu được sử dụng để so khớp các tổ hợp ký tự trong chuỗi. Lập trình viên sử dụng chúng để tìm kiếm, chỉnh sửa hoặc xác nhận văn bản bởi vì chúng chính xác và hiệu quả.

## Làm thế nào:
Hãy cùng tìm hiểu một số cơ bản về regex trong Ruby.

```Ruby
# Tìm kiếm một khớp
câu = "Hello, World!"
puts câu.match(/World/) # Đầu ra: World

# Thay thế
puts câu.gsub(/World/, "Ruby") # Đầu ra: Hello, Ruby!

# Trích xuất khớp
email = "contact@example.com"
puts email.match(/\A[^@\s]+@([^@\s]+\.)+[^@\s]+\z/).to_s # Đầu ra: contact@example.com

# Lặp qua các khớp
"Frodo, Gandalf, Arwen".scan(/\w+/) { |tên| puts tên }
# Đầu ra:
# Frodo
# Gandalf
# Arwen
```

## Sâu hơn nữa
Biểu thức chính quy trong Ruby đã được ảnh hưởng bởi khả năng regex mạnh mẽ của Perl. Các phương án thay thế cho regex bao gồm các phương thức chuỗi như `#include?`, `#start_with?`, và `#end_with?`, nhưng không cái nào cung cấp cùng một sức mạnh và linh hoạt. Ruby triển khai regex sử dụng thư viện riêng của mình được phát triển từ engine regex của Perl, cung cấp các tính năng như nhìn trước và nhìn sau, khớp không tham lam và các phím tắt của lớp ký tự.

## Xem thêm
- [Biểu Thức Chính Quy Ruby](https://ruby-doc.org/core-3.1.0/Regexp.html): Tài liệu chính thức Ruby về regex.
- [Rubular](http://rubular.com/): Bộ biên tập biểu thức chính quy dựa trên Ruby, tốt cho việc kiểm tra các mẫu.
