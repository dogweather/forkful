---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:29.781501-07:00
description: "L\xE0m th\u1EBF n\xE0o: \u0110\u1EC3 l\u1EA5y c\xE1c \u0111\u1ED1i s\u1ED1\
  \ d\xF2ng l\u1EC7nh, Ruby cung c\u1EA5p m\u1ED9t m\u1EA3ng \u0111\u01A1n gi\u1EA3\
  n: `ARGV`. N\xF3 ch\u1EE9a t\u1EA5t c\u1EA3 c\xE1c \u0111\u1ED1i s\u1ED1 \u0111\u01B0\
  \u1EE3c truy\u1EC1n v\xE0o, theo th\u1EE9 t\u1EF1 ch\xFAng \u0111\u01B0\u1EE3c \u0111\
  \u01B0a\u2026"
lastmod: '2024-03-13T22:44:37.361764-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1EC3 l\u1EA5y c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh, Ruby\
  \ cung c\u1EA5p m\u1ED9t m\u1EA3ng \u0111\u01A1n gi\u1EA3n."
title: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh"
weight: 23
---

## Làm thế nào:
Để lấy các đối số dòng lệnh, Ruby cung cấp một mảng đơn giản: `ARGV`. Nó chứa tất cả các đối số được truyền vào, theo thứ tự chúng được đưa ra.

```Ruby
# hello.rb
name = ARGV[0] || "Thế giới"
puts "Xin chào, #{name}!"

# Chạy bằng: ruby hello.rb Alice
# Đầu ra: Xin chào, Alice!
```

Để xử lý nhiều đối số:

```Ruby
# greet.rb
name, time_of_day = ARGV
puts "Chào buổi #{time_of_day || 'ngày'}, #{name || 'bạn'}!"

# Chạy bằng: ruby greet.rb Bob Buổi sáng
# Đầu ra: Chào buổi Buổi sáng, Bob!
```

Tạo tùy chọn với một vòng lặp:

```Ruby
# options.rb
options = {}
ARGV.each do |arg|
  key, value = arg.split('=')
  options[key.to_sym] = value
end
p options

# Chạy bằng: ruby options.rb name=Tom age=30
# Đầu ra: {:name=>"Tom", :age=>"30"}
```

## Sâu hơn
Đọc các đối số dòng lệnh là một thực hành cũ như chính giao diện dòng lệnh. Nó liên quan đến việc sử dụng đầu vào của người dùng mà không cần GUI—cần thiết cho tự động hóa hoặc khi chạy kịch bản trên máy chủ.

`ARGV` của Ruby không phải là duy nhất; nhiều ngôn ngữ có điều gì đó tương tự. Tuy nhiên, cài đặt của Ruby tập trung vào sự đơn giản và cú pháp rõ ràng—không rắc rối, chỉ là một mảng.

Bên dưới bề mặt, `ARGV` chỉ là một thực thể của `Array` được điền trước với các đối số xuất hiện sau tên kịch bản trong lời gọi lệnh. Ruby thiết lập nó trước khi mã của bạn thậm chí được chạy, làm cho nó ngay lập tức sẵn sàng để sử dụng.

Có phương án thay thế? Chắc chắn. Đối với nhu cầu phức tạp, như phân tích cờ (ví dụ, `--verbose` hay `-v`), Ruby có lớp `OptionParser` trong thư viện chuẩn. Nó có thể xử lý nhiều hơn `ARGV`, như giá trị mặc định, chuyển đổi kiểu tự động và tạo thông điệp trợ giúp.

Đôi khi, bạn chỉ muốn biết liệu một đối số có được cung cấp hay không, bỏ qua giá trị của nó. Với mục đích đó, `ARGV.include?` giải quyết vấn đề.

## Xem thêm
- Giới thiệu về `OptionParser`: [https://ruby-doc.org/stdlib-2.7.0/libdoc/optparse/rdoc/OptionParser.html](https://ruby-doc.org/stdlib-2.7.0/libdoc/optparse/rdoc/OptionParser.html)
- Thêm về các đối số dòng lệnh trong Ruby: [https://www.rubyguides.com/2018/12/ruby-argv/](https://www.rubyguides.com/2018/12/ruby-argv/)
