---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:17.391771-07:00
description: "Tr\xEDch xu\u1EA5t chu\u1ED7i con l\xE0 vi\u1EC7c l\u1EA5y ra c\xE1\
  c ph\u1EA7n c\u1EE5 th\u1EC3 c\u1EE7a v\u0103n b\u1EA3n t\u1EEB m\u1ED9t chu\u1ED7\
  i. L\u1EADp tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3\
  \ thao t\xE1c v\xE0 s\u1EED d\u1EE5ng c\xE1c ph\u1EA7n d\u1EEF li\u1EC7u -\u2026"
lastmod: '2024-03-13T22:44:37.318163-06:00'
model: gpt-4-0125-preview
summary: "Tr\xEDch xu\u1EA5t chu\u1ED7i con l\xE0 vi\u1EC7c l\u1EA5y ra c\xE1c ph\u1EA7\
  n c\u1EE5 th\u1EC3 c\u1EE7a v\u0103n b\u1EA3n t\u1EEB m\u1ED9t chu\u1ED7i. L\u1EAD\
  p tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u n\xE0y \u0111\u1EC3 thao t\xE1\
  c v\xE0 s\u1EED d\u1EE5ng c\xE1c ph\u1EA7n d\u1EEF li\u1EC7u -\u2026"
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
weight: 6
---

## Cái gì & Tại sao?

Trích xuất chuỗi con là việc lấy ra các phần cụ thể của văn bản từ một chuỗi. Lập trình viên thực hiện điều này để thao tác và sử dụng các phần dữ liệu - như lấy tên người dùng từ địa chỉ email hoặc phân tích cú pháp ngày tháng từ dấu thời gian.

## Cách thực hiện:

Ruby làm cho việc trích xuất chuỗi con trở nên đơn giản. Hãy đi thẳng vào vấn đề:

```Ruby
str = "Hello, Ruby World!"

# Phương pháp 1: Sử dụng chỉ số mảng
substring = str[7, 4] # "Ruby"
puts substring

# Phương pháp 2: Sử dụng phương thức slice
slice = str.slice(7, 4) # "Ruby"
puts slice

# Phương pháp 3: Biểu thức chính quy
match = str[/[Rr]uby/] # "Ruby"
puts match

# Phương pháp 4: Tách và truy cập mảng
split_array = str.split # mặc định tách theo khoảng trắng
picked_word = split_array[2] # "World!"
puts picked_word
```

Kết quả mẫu cho mỗi đoạn mã sẽ lần lượt là "Ruby", "Ruby", "Ruby", "World!".

## Khám phá sâu hơn

Ngày xưa, việc trích xuất chuỗi con là một quá trình dài dòng hơn. Ruby đã phát triển, mặc dù. Ngày nay, bạn có các phương thức và regex để sử dụng.

Đây là những gì đang diễn ra bên dưới: `[7, 4]` có nghĩa là bắt đầu từ ký tự thứ 7 và lấy tiếp theo 4 ký tự. `slice` chỉ là một cách phương pháp hóa để nói cùng một điều đó. Với regex, `/[Rr]uby/` giống như nói, "Bắt cho tôi một 'Ruby' hoặc 'ruby', tùy cái nào bạn tìm thấy trước." Phương thức `split` chia chuỗi thành một mảng tại mỗi khoảng trắng, và `[2]` chọn từ thứ ba - hãy nhớ rằng, mảng bắt đầu từ số không.

Có phương thức thay thế không? Chắc chắn rồi, Ruby có chúng. `partition`, `rpartition`, và `match` cũng có thể đóng vai trò ở đây. Mỗi phương thức có trường hợp sử dụng riêng nhưng việc biết `.slice` và regex đã đủ để đáp ứng hầu hết nhu cầu.

Nói tóm lại: việc trích xuất chuỗi con đề cập đến việc thao tác văn bản một cách chính xác. Công cụ đúng đắn nghĩa là mã sạch và hiệu quả.

## Xem thêm

- Tài liệu Ruby về Chuỗi: [ruby-doc.org/core-2.7.0/String.html](https://ruby-doc.org/core-2.7.0/String.html)
- Biểu thức chính quy trong Ruby: [ruby-doc.org/core-2.7.0/Regexp.html](https://ruby-doc.org/core-2.7.0/Regexp.html)
- Hướng dẫn Phong cách Ruby về Chuỗi: [rubystyle.guide/#strings](https://rubystyle.guide/#strings)
