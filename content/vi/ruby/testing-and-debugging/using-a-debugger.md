---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:28.104204-07:00
description: "Ruby \u0111i k\xE8m v\u1EDBi m\u1ED9t debugger t\xEDch h\u1EE3p s\u1EB5\
  n c\xF3 t\xEAn l\xE0 `byebug`. \u0110\u1EA7u ti\xEAn, bao g\u1ED3m `byebug` trong\
  \ Gemfile c\u1EE7a b\u1EA1n v\xE0 ch\u1EA1y `bundle install`. Sau \u0111\xF3, th\u1EA3\
  \u2026"
lastmod: '2024-03-13T22:44:37.347036-06:00'
model: gpt-4-0125-preview
summary: "Ruby \u0111i k\xE8m v\u1EDBi m\u1ED9t debugger t\xEDch h\u1EE3p s\u1EB5\
  n c\xF3 t\xEAn l\xE0 `byebug`. \u0110\u1EA7u ti\xEAn, bao g\u1ED3m `byebug` trong\
  \ Gemfile c\u1EE7a b\u1EA1n v\xE0 ch\u1EA1y `bundle install`. Sau \u0111\xF3, th\u1EA3\
  \u2026"
title: "S\u1EED d\u1EE5ng b\u1ED9 g\u1EE1 l\u1ED7i"
weight: 35
---

## Cách thực hiện:
Ruby đi kèm với một debugger tích hợp sẵn có tên là `byebug`. Đầu tiên, bao gồm `byebug` trong Gemfile của bạn và chạy `bundle install`. Sau đó, thả `byebug` ngay tại chỗ bạn muốn chương trình của mình tạm dừng.

```Ruby
require 'byebug'

def calculate_magic(number)
  byebug
  magic_number = number * 7
  return magic_number
end

puts calculate_magic(6)
```

Khi chạy script này, thực hiện sẽ tạm dừng tại `byebug`, và bạn sẽ được đưa vào một phiên làm việc tương tác nơi bạn có thể nhập các lệnh như:

```
step
next
continue
var local
```

Đầu ra mẫu sẽ cho bạn giao diện như sau:

```
[2, 11] in example.rb
    2: 
    3: def calculate_magic(number)
    4:   byebug
=>  5:   magic_number = number * 7
    6:   return magic_number
    7: end
    8: 
    9: puts calculate_magic(6)
(byebug) 
```

## Sâu hơn:
Trước `byebug`, người dùng Ruby đã sử dụng `debugger` và `pry`. Cái sau, `pry`, hơn một debugger; nó là một REPL mạnh mẽ cũng có thể được sử dụng để debug với điểm ngắt `binding.pry`.

Các phương án thay thế cho `byebug` của Ruby bao gồm `pry-byebug`, kết hợp chức năng `pry` với `byebug`, và `ruby-debug`, là một gem cũ không được bảo trì tích cực.

Khi bạn kích hoạt `byebug`, debugger tạm dừng việc thực thi mã của bạn và cho phép bạn nhìn qua quá trình thực thi. Bạn có thể thấy và thay đổi các biến, nhảy đến các điểm khác nhau trong mã, và thậm chí chạy một số dòng mã Ruby từng dòng một. Nó giống như có khả năng du hành thời gian với mã Ruby của bạn.

## Xem thêm:
- Kho GitHub của Byebug: [https://github.com/deivid-rodriguez/byebug](https://github.com/deivid-rodriguez/byebug)
- Tài liệu về Pry: [https://github.com/pry/pry](https://github.com/pry/pry)
- Hướng dẫn Debug các Ứng dụng Rails: [https://guides.rubyonrails.org/debugging_rails_applications.html](https://guides.rubyonrails.org/debugging_rails_applications.html)
