---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:58.843349-07:00
description: "L\xE0m th\u1EBF n\xE0o: H\xE3y c\xF9ng t\xECm hi\u1EC3u m\u1ED9t s\u1ED1\
  \ c\u01A1 b\u1EA3n v\u1EC1 regex trong Ruby."
lastmod: '2024-03-13T22:44:37.323749-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y c\xF9ng t\xECm hi\u1EC3u m\u1ED9t s\u1ED1 c\u01A1 b\u1EA3n v\u1EC1\
  \ regex trong Ruby."
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
weight: 11
---

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
