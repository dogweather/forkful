---
title:                "Nội suy chuỗi ký tự"
date:                  2024-01-28T22:02:52.137934-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nội suy chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Nội suy chuỗi cho phép bạn nhúng biến hoặc biểu thức bên trong một chuỗi. Chúng ta làm điều này để có được mã nguồn sạch hơn, dễ đọc hơn, kết hợp nội dung động với văn bản tĩnh.

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
