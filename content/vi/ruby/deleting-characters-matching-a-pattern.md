---
title:                "Xóa các ký tự phù hợp với một mẫu"
date:                  2024-01-28T21:59:40.357986-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xóa các ký tự phù hợp với một mẫu"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái Gì & Tại Sao?
Xoá các ký tự phù hợp với một mẫu trong chuỗi là việc xoá bỏ những phần bạn không cần, như là loại bỏ hashtag từ các tweet. Lập trình viên làm việc này để làm sạch dữ liệu, định dạng nó một cách nhất quán, hoặc chuẩn bị nó cho quá trình xử lý tiếp theo.

## Cách thực hiện:
```Ruby
# Xoá đơn giản sử dụng String#gsub
example = "Hello, #World!"
cleaned_example = example.gsub(/#/, '') # => "Hello, World!"

puts cleaned_example # Đầu ra: Hello, World!

# Xoá một dãy ký tự
sequence_example = "Th1s is 2 an example3."
cleaned_sequence = sequence_example.gsub(/[0-9]/, '') # => "This is an example."

puts cleaned_sequence # Đầu ra: This is an example.

# Xoá bằng cách sử dụng String#delete
delete_example = "Remove vowels from this line."
cleaned_delete = delete_example.delete('aeiou') # => "Rmv vwls frm ths ln."

puts cleaned_delete # Đầu ra: Rmv vwls frm ths ln.
```

## Sâu hơn
Trong lịch sử, Ruby đã là một ngôn ngữ tập trung mạnh mẽ vào xử lý văn bản, kế thừa một số triết lý từ Perl. Đó là lý do tại sao nó cung cấp cho bạn các công cụ như `gsub` và `delete` ngay từ đầu.

`gsub` là viết tắt của global substitution (thay thế toàn cục). Nó thường được sử dụng để thay thế các phần của chuỗi phù hợp với một mẫu (biểu thức chính quy) bằng một chuỗi khác. Khi được cung cấp một chuỗi thay thế trống, nó hiệu quả xoá các ký tự phù hợp.

`delete` ít linh hoạt hơn `gsub` nhưng nhanh hơn khi bạn chỉ muốn loại bỏ các ký tự cụ thể. Bạn không thể sử dụng biểu thức chính quy với `delete`, nhưng cho việc loại bỏ ký tự đơn giản, đây là lựa chọn rõ ràng.

Có những cách khác để thực hiện việc này, thư viện như `scan` và `split` có thể phân tích chuỗi, và sau đó bạn có thể tái tạo chúng mà không cần những ký tự không mong muốn. Nhưng cho việc xoá ký tự trực tiếp, `gsub` và `delete` là các bạn tốt nhất của bạn.

## Xem Thêm
- Tài liệu `gsub` của Ruby: [Ruby Doc gsub](https://ruby-doc.org/core-3.1.0/String.html#method-i-gsub)
- Tài liệu `delete` của Ruby: [Ruby Doc delete](https://ruby-doc.org/core-3.1.0/String.html#method-i-delete)
- Biểu thức chính quy trong Ruby: [Ruby Regexp](https://ruby-doc.org/core-3.1.0/Regexp.html)
- "Programming Ruby: The Pragmatic Programmer’s Guide" để hiểu sâu hơn về khả năng xử lý văn bản của Ruby.
