---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:29.781501-07:00
description: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh trong Ruby\
  \ cho ph\xE9p c\xE1c k\u1ECBch b\u1EA3n nh\u1EADn \u0111\u1EA7u v\xE0o ngay khi\
  \ ch\xFAng \u0111\u01B0\u1EE3c ch\u1EA1y, nh\u01B0 c\u1EA5u h\xECnh t\xF9y ch\u1ECD\
  n ho\u1EB7c truy\u1EC1n d\u1EEF li\u1EC7u. C\xE1c l\u1EADp tr\xECnh\u2026"
lastmod: '2024-03-13T22:44:37.361764-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh trong Ruby\
  \ cho ph\xE9p c\xE1c k\u1ECBch b\u1EA3n nh\u1EADn \u0111\u1EA7u v\xE0o ngay khi\
  \ ch\xFAng \u0111\u01B0\u1EE3c ch\u1EA1y, nh\u01B0 c\u1EA5u h\xECnh t\xF9y ch\u1ECD\
  n ho\u1EB7c truy\u1EC1n d\u1EEF li\u1EC7u. C\xE1c l\u1EADp tr\xECnh\u2026"
title: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh"
---

{{< edit_this_page >}}

## Cái gì và Tại sao?
Đọc các đối số dòng lệnh trong Ruby cho phép các kịch bản nhận đầu vào ngay khi chúng được chạy, như cấu hình tùy chọn hoặc truyền dữ liệu. Các lập trình viên sử dụng chúng để làm cho các kịch bản trở nên động và có thể thích ứng mà không cần cố định giá trị.

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
