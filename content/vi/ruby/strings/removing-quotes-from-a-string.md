---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:35.275284-07:00
description: "Lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c k\xE9p kh\u1ECFi m\u1ED9t chu\u1ED7\
  i c\xF3 ngh\u0129a l\xE0 lo\u1EA1i b\u1ECF nh\u1EEFng d\u1EA5u ngo\u1EB7c k\xE9\
  p ho\u1EB7c d\u1EA5u ngo\u1EB7c \u0111\u01A1n bao quanh c\xE1c gi\xE1 tr\u1ECB v\u0103\
  n b\u1EA3n. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u01B0\u1EDDng\u2026"
lastmod: 2024-02-19 22:04:56.549027
model: gpt-4-0125-preview
summary: "Lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c k\xE9p kh\u1ECFi m\u1ED9t chu\u1ED7\
  i c\xF3 ngh\u0129a l\xE0 lo\u1EA1i b\u1ECF nh\u1EEFng d\u1EA5u ngo\u1EB7c k\xE9\
  p ho\u1EB7c d\u1EA5u ngo\u1EB7c \u0111\u01A1n bao quanh c\xE1c gi\xE1 tr\u1ECB v\u0103\
  n b\u1EA3n. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u01B0\u1EDDng\u2026"
title: "Lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c k\xE9p kh\u1ECFi chu\u1ED7i"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Loại bỏ dấu ngoặc kép khỏi một chuỗi có nghĩa là loại bỏ những dấu ngoặc kép hoặc dấu ngoặc đơn bao quanh các giá trị văn bản. Các lập trình viên thường làm điều này để làm sạch đầu vào từ người dùng, đảm bảo tính nhất quán trong việc xử lý dữ liệu, hoặc chuẩn bị dữ liệu cho các hệ thống có thể bị nhầm lẫn bởi những ký tự thừa.

## Làm thế nào:
Ruby có một số thủ thuật hay ho để cắt bỏ những dấu ngoặc kép phiền phức đó. Bạn có thể sử dụng các phương thức `gsub` hoặc `delete` để hoàn thành công việc. Dưới đây là một số mã để tham khảo:

```ruby
# Sử dụng gsub để loại bỏ dấu ngoặc kép và dấu ngoặc đơn
quoted_string = "\"Say 'hello' to my little friend!\""
unquoted_string = quoted_string.gsub(/'|"/, '')
puts unquoted_string 
# Đầu ra: Say hello to my little friend!

# Nếu bạn biết mình chỉ xử lý một loại dấu ngoặc
single_quoted_string = "'Stay a while and listen!'"
clean_string = single_quoted_string.delete("'")
puts clean_string 
# Đầu ra: Stay a while and listen!
```

## Tìm hiểu sâu
Lịch sử của dấu ngoặc kép quay trở lại những ngày đầu của lập trình, nơi chúng thường được sử dụng làm dấu phân cách chuỗi. Ngày nay, như trước đây, bạn có thể thấy mình cần loại bỏ những ký tự dấu ngoặc khi chúng không cần thiết hoặc khi chúng có thể làm gián đoạn việc lưu trữ và thao tác dữ liệu.

Chúng tôi đã nói về `gsub` và `delete` nhưng cũng có các phương thức khác, như `tr` hoặc `tr_s`, cho bạn nhiều kiểm soát hơn hoặc có thể xử lý một số trường hợp sử dụng khác nhau:

```ruby
# tr cũng có thể loại bỏ dấu ngoặc kép
double_quoted_string = "\"Do or do not, there is no try.\""
clean_string = double_quoted_string.tr('\"', '')
puts clean_string 
# Đầu ra: Do or do not, there is no try.
```

Hãy nhớ, mỗi phương thức này có trường hợp sử dụng của riêng mình. `gsub` mạnh mẽ hơn khi bạn đang xử lý với các mẫu phức tạp hoặc nhiều thay thế. `delete` và `tr` hoạt động tốt đẹp cho việc loại bỏ ký tự đơn giản, thẳng thắn.

## Tham khảo thêm
Để đọc thêm, và để xem các phương thức này được áp dụng trong các cơ sở mã lớn hơn, hãy tham khảo:
- Tài liệu Ruby cho [String#gsub](https://ruby-doc.org/core-3.1.2/String.html#method-i-gsub), [String#delete](https://ruby-doc.org/core-3.1.2/String.html#method-i-delete), và [String#tr](https://ruby-doc.org/core-3.1.2/String.html#method-i-tr).
- Ruby Monstas có một bộ bài tập [String](http://ruby-for-beginners.rubymonstas.org/built_in_classes/strings.html) tuyệt vời, bao gồm cả việc làm việc với dấu ngoặc.
- Các cuộc thảo luận trên Stack Overflow về [manipulation string](https://stackoverflow.com/search?q=ruby+remove+quotes+from+string) cung cấp vấn đề và giải pháp thực tế từ các Rubyist.
