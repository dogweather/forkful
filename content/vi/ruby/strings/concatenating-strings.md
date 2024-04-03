---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:07.430398-07:00
description: "N\u1ED1i chu\u1ED7i ch\u1EC9 l\xE0 c\xE1ch n\xF3i c\u1EA7u k\u1EF3 c\u1EE7\
  a vi\u1EC7c 'gh\xE9p ch\xFAng l\u1EA1i v\u1EDBi nhau t\u1EEB \u0111\u1EA7u \u0111\
  \u1EBFn cu\u1ED1i'. C\xE1c l\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0\
  y \u0111\u1EC3 k\u1EBFt h\u1EE3p t\u1EEB v\xE0 c\xE2u, x\xE2y d\u1EF1ng th\xF4ng\u2026"
lastmod: '2024-03-13T22:44:37.326465-06:00'
model: gpt-4-0125-preview
summary: "N\u1ED1i chu\u1ED7i ch\u1EC9 l\xE0 c\xE1ch n\xF3i c\u1EA7u k\u1EF3 c\u1EE7\
  a vi\u1EC7c 'gh\xE9p ch\xFAng l\u1EA1i v\u1EDBi nhau t\u1EEB \u0111\u1EA7u \u0111\
  \u1EBFn cu\u1ED1i'."
title: "N\u1ED1i chu\u1ED7i k\xFD t\u1EF1"
weight: 3
---

## Gì và Tại sao?
Nối chuỗi chỉ là cách nói cầu kỳ của việc 'ghép chúng lại với nhau từ đầu đến cuối'. Các lập trình viên làm điều này để kết hợp từ và câu, xây dựng thông điệp, hoặc để chèn động các giá trị vào văn bản.

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
