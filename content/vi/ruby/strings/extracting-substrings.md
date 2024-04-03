---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:17.391771-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Ruby l\xE0m cho vi\u1EC7c tr\xEDch xu\u1EA5\
  t chu\u1ED7i con tr\u1EDF n\xEAn \u0111\u01A1n gi\u1EA3n. H\xE3y \u0111i th\u1EB3\
  ng v\xE0o v\u1EA5n \u0111\u1EC1."
lastmod: '2024-03-13T22:44:37.318163-06:00'
model: gpt-4-0125-preview
summary: "Ruby l\xE0m cho vi\u1EC7c tr\xEDch xu\u1EA5t chu\u1ED7i con tr\u1EDF n\xEA\
  n \u0111\u01A1n gi\u1EA3n."
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
weight: 6
---

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
