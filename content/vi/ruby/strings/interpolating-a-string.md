---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:52.137934-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong Ruby, b\u1EA1n bao bi\u1EBFn ho\u1EB7\
  c bi\u1EC3u th\u1EE9c c\u1EE7a m\xECnh trong `#{}` v\xE0 \u0111\u1EB7t n\xF3 v\xE0\
  o n\u01A1i b\u1EA1n mu\u1ED1n trong m\u1ED9t chu\u1ED7i \u0111\u01B0\u1EE3c bao\
  \ b\u1EDFi d\u1EA5u ngo\u1EB7c k\xE9p. Nh\u01B0\u2026"
lastmod: '2024-03-13T22:44:37.306245-06:00'
model: gpt-4-0125-preview
summary: "Trong Ruby, b\u1EA1n bao bi\u1EBFn ho\u1EB7c bi\u1EC3u th\u1EE9c c\u1EE7\
  a m\xECnh trong `#{}` v\xE0 \u0111\u1EB7t n\xF3 v\xE0o n\u01A1i b\u1EA1n mu\u1ED1\
  n trong m\u1ED9t chu\u1ED7i \u0111\u01B0\u1EE3c bao b\u1EDFi d\u1EA5u ngo\u1EB7\
  c k\xE9p."
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
weight: 8
---

## Cách thực hiện:
Trong Ruby, bạn bao biến hoặc biểu thức của mình trong `#{}` và đặt nó vào nơi bạn muốn trong một chuỗi được bao bởi dấu ngoặc kép. Như sau:

```Ruby
name = "Jesse"
greeting = "Hey there, #{name}!"
puts greeting # => Hey there, Jesse!
```

Bạn không chỉ giới hạn ở biến; bất kỳ mã Ruby nào cũng có thể được đặt vào đó:

```Ruby
price_per_kg = 5
quantity = 2
puts "Your total is: $#{price_per_kg * quantity}" # => Your total is: $10
```

Nhớ rằng, dấu ngoặc đơn không có tác dụng:

```Ruby
puts 'Hey there, #{name}!' # => Hey there, \#{name}!
```

## Sâu hơn nữa
Trước đây, chúng ta thường nối chuỗi và biến sử dụng `+` hoặc `<<`, khiến mọi thứ trở nên lộn xộn nhanh chóng.

```Ruby
email = "user" + "@" + "example.com"
```

Sự xuất hiện của nội suy chuỗi trong Ruby, một cách tinh tế hơn để hợp nhất văn bản với mã. Ruby đánh giá bất cứ thứ gì bên trong `#{}` và tự động chuyển nó thành một chuỗi. Xem xét công việc nó lưu từ việc chuyển đổi và nối chuỗi:

```Ruby
"pi là xấp xỉ #{Math::PI.round(2)}"
```

Ruby không phải là duy nhất; nhiều ngôn ngữ có biến thể riêng của tính năng tiện lợi này. Nhưng cẩn thận: không giống như một số ngôn ngữ, Ruby nghiêm ngặt dành phép màu này cho chuỗi được bao bởi dấu ngoặc kép và một số trường hợp khác (như dấu hoàn lưu và ký hiệu). Dấu ngoặc đơn chỉ đơn giản là đưa ra những gì bên trong chúng, kể cả ngoặc nhọn.

## Tham khảo
- Tài liệu Ruby về cú pháp: [Tài liệu Ruby - Cú pháp](https://ruby-doc.org/core-3.1.2/doc/syntax/literals_rdoc.html#label-Strings)
- Cái nhìn sâu hơn vào việc thao tác chuỗi: [Ruby-Doc.org - Chuỗi](https://ruby-doc.org/core-3.1.2/String.html)
