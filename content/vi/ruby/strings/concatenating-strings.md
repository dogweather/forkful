---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:07.430398-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong Ruby, b\u1EA1n c\xF3 th\u1EC3 n\u1ED1\
  i chu\u1ED7i b\u1EB1ng to\xE1n t\u1EED `+` ho\u1EB7c ph\u01B0\u01A1ng th\u1EE9c\
  \ `<<`, ph\u01B0\u01A1ng th\u1EE9c n\xE0y s\u1EEDa \u0111\u1ED5i chu\u1ED7i ngay\
  \ t\u1EA1i ch\u1ED7. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch \u0111\u1EC3 k\u1EBF\
  t\u2026"
lastmod: '2024-03-13T22:44:37.326465-06:00'
model: gpt-4-0125-preview
summary: "Trong Ruby, b\u1EA1n c\xF3 th\u1EC3 n\u1ED1i chu\u1ED7i b\u1EB1ng to\xE1\
  n t\u1EED `+` ho\u1EB7c ph\u01B0\u01A1ng th\u1EE9c `<<`, ph\u01B0\u01A1ng th\u1EE9\
  c n\xE0y s\u1EEDa \u0111\u1ED5i chu\u1ED7i ngay t\u1EA1i ch\u1ED7."
title: "N\u1ED1i chu\u1ED7i k\xFD t\u1EF1"
weight: 3
---

## Làm thế nào:
Trong Ruby, bạn có thể nối chuỗi bằng toán tử `+` hoặc phương thức `<<`, phương thức này sửa đổi chuỗi ngay tại chỗ. Dưới đây là cách để kết nối các dấu chấm—hay chính xác hơn, là các từ:

```Ruby
# Sử dụng toán tử +, trả về một chuỗi mới
greeting = "Hello, " + "world!"
puts greeting # Đầu ra: Hello, world!

# Sử dụng phương thức <<, thay đổi chuỗi gốc
name = "Alice"
name << ", meet Bob"
puts name # Đầu ra: Alice, meet Bob
```

## Đi sâu hơn
Việc nối chuỗi đã có trong Ruby từ khi nó ra đời. Nhưng với thời gian, ngôn ngữ đã cung cấp thêm nhiều cách để dệt các chuỗi lại với nhau.

Chúng ta đã đề cập đến `+` và `<<`, nhưng còn có phương thức `concat` và nội suy.

- Sử dụng `concat`: Phương thức này giống như `<<` nhưng cho phép bạn gắn thêm nhiều chuỗi cùng một lúc.
```Ruby
phrase = "Roses are red"
phrase.concat(", violets are blue")
puts phrase # Đầu ra: Roses are red, violets are blue
```

- Nội suy: Chèn biến vào một chuỗi mà không cần nối chúng một cách trực tiếp. Nó gọn gàng hơn và được ưa chuộng để chèn biến:
```Ruby
mood = "excited"
message = "I am #{mood} to learn Ruby!"
puts message # Đầu ra: I am excited to learn Ruby!
```

Nội suy tự động gọi phương thức `to_s` trên bất kỳ biến nào, đảm bảo các kiểu không phải chuỗi có thể được xử lý một cách dễ dàng trong chuỗi.

Hơn nữa, hãy nhớ—không chỉ là việc ghép từ lại với nhau; Ruby cũng chú ý đến hiệu suất. Khi bạn sử dụng `+`, Ruby tạo một chuỗi mới. Theo thời gian hoặc trong các vòng lặp, điều này có thể tiêu tốn bộ nhớ. Ngược lại, `<<` và `concat` chỉnh sửa chuỗi gốc, thường hiệu quả hơn.

## Xem thêm
- Tài liệu Ruby về String: https://ruby-doc.org/core-3.1.2/String.html
- Bài viết về nội suy chuỗi Ruby: https://www.rubyguides.com/2018/11/ruby-string-interpolation/
- Hướng dẫn về các toán tử Ruby: https://www.tutorialspoint.com/ruby/ruby_operators.htm
