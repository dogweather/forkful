---
title:                "Trích xuất chuỗi con"
date:                  2024-01-28T22:00:17.391771-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trích xuất chuỗi con"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
