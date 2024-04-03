---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:32.776345-07:00
description: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n l\xE0 vi\u1EC7\
  c \u0111\u1ED5i c\xE1c t\u1EEB ho\u1EB7c c\u1EE5m t\u1EEB c\u1EE5 th\u1EC3 th\xE0\
  nh c\xE1c l\u1EF1a ch\u1ECDn kh\xE1c trong m\u1ED9t chu\u1ED7i. C\xE1c l\u1EADp\
  \ tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 c\u1EAD\
  p\u2026"
lastmod: '2024-03-13T22:44:37.304912-06:00'
model: gpt-4-0125-preview
summary: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n l\xE0 vi\u1EC7c \u0111\
  \u1ED5i c\xE1c t\u1EEB ho\u1EB7c c\u1EE5m t\u1EEB c\u1EE5 th\u1EC3 th\xE0nh c\xE1\
  c l\u1EF1a ch\u1ECDn kh\xE1c trong m\u1ED9t chu\u1ED7i."
title: "T\xECm ki\u1EBFm v\xE0 thay th\u1EBF v\u0103n b\u1EA3n"
weight: 10
---

## Cái gì và Tại sao?
Tìm kiếm và thay thế văn bản là việc đổi các từ hoặc cụm từ cụ thể thành các lựa chọn khác trong một chuỗi. Các lập trình viên thực hiện điều này để cập nhật dữ liệu, sửa lỗi, hoặc thay đổi định dạng - nó giữ cho mọi thứ chính xác và ngăn nắp.

## Cách thực hiện:
Ruby làm cho điều này trở nên dễ dàng. Sử dụng `gsub` để thay thế toàn cầu, hoặc `sub` cho một trường hợp duy nhất. Dưới đây là một cái nhìn nhanh:

```ruby
# Chuỗi gốc
phrase = "Hello, world!"

# Thay thế 'world' bằng 'Ruby'
puts phrase.gsub('world', 'Ruby')
# => Hello, Ruby!

# Chỉ thay thế sự xuất hiện đầu tiên của 'l'
puts phrase.sub('l', '7')
# => He7lo, world!
```
Kết quả? Lần in thứ nhất hiển thị `"Hello, Ruby!"`, lần thứ hai hiển thị `"He7lo, world!"`.

## Sâu hơn
Các phương thức `gsub` và `sub` đã đi cùng Ruby từ những ngày đầu, phản ánh khái niệm thay thế từ các ngôn ngữ lâu đời như Perl. Có sự thay thế? Chắc chắn, bạn có thể sử dụng regex cho các mẫu phức tạp hơn, hoặc thậm chí ghép lại `split` và `join` nếu bạn cảm thấy khéo léo.

Điều gì làm cho điều này thú vị là khả năng sử dụng block của Ruby với `gsub`. Thay vì thay thế đơn giản, bạn có thể làm một số công việc nặng nhọc bên trong block đó:

```ruby
# Viết hoa từng từ
puts "make me pretty".gsub(/\b\w/) { |match| match.upcase }
# => Make Me Pretty
```

Tại sao phải bận tâm? Đầu tiên, sử dụng regex với `gsub` cho phép bạn giải quyết những trường hợp tinh tế mà bạn cần nhiều sự tinh tế hơn là chỉ 'tìm cái này, thay thế bằng cái kia'.

## Xem thêm
Mài giũa kỹ năng của bạn - đắm mình vào tài liệu hoặc xem những tài nguyên sau:
- [Tài liệu Ruby String#gsub](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
- [Biểu thức Chính quy trong Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)

Hiểu rồi đúng không? Tốt. Giờ thì đi chơi với một số chuỗi nào.
