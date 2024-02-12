---
title:                "Tìm kiếm và thay thế văn bản"
aliases: - /vi/ruby/searching-and-replacing-text.md
date:                  2024-01-28T22:07:32.776345-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm kiếm và thay thế văn bản"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/searching-and-replacing-text.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
