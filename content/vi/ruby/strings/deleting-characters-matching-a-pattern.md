---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:40.357986-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong l\u1ECBch s\u1EED, Ruby \u0111\xE3\
  \ l\xE0 m\u1ED9t ng\xF4n ng\u1EEF t\u1EADp trung m\u1EA1nh m\u1EBD v\xE0o x\u1EED\
  \ l\xFD v\u0103n b\u1EA3n, k\u1EBF th\u1EEBa m\u1ED9t s\u1ED1 tri\u1EBFt l\xFD t\u1EEB\
  \ Perl. \u0110\xF3 l\xE0 l\xFD do t\u1EA1i sao n\xF3 cung\u2026"
lastmod: '2024-04-05T21:53:38.644921-06:00'
model: gpt-4-0125-preview
summary: "Trong l\u1ECBch s\u1EED, Ruby \u0111\xE3 l\xE0 m\u1ED9t ng\xF4n ng\u1EEF\
  \ t\u1EADp trung m\u1EA1nh m\u1EBD v\xE0o x\u1EED l\xFD v\u0103n b\u1EA3n, k\u1EBF\
  \ th\u1EEBa m\u1ED9t s\u1ED1 tri\u1EBFt l\xFD t\u1EEB Perl."
title: "X\xF3a c\xE1c k\xFD t\u1EF1 ph\xF9 h\u1EE3p v\u1EDBi m\u1ED9t m\u1EABu"
weight: 5
---

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
