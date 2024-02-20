---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:28.104204-07:00
description: "S\u1EED d\u1EE5ng debugger trong Ruby mang l\u1EA1i cho l\u1EADp tr\xEC\
  nh vi\xEAn m\u1ED9t si\xEAu n\u0103ng l\u1EF1c \u0111\u1EC3 t\u1EA1m d\u1EEBng m\xE3\
  \ c\u1EE7a h\u1ECD, ki\u1EC3m tra c\xE1c bi\u1EBFn v\xE0 b\u01B0\u1EDBc qua m\xE3\
  \ c\u1EE7a h\u1ECD t\u1EEBng d\xF2ng m\u1ED9t.\u2026"
lastmod: 2024-02-19 22:04:56.576565
model: gpt-4-0125-preview
summary: "S\u1EED d\u1EE5ng debugger trong Ruby mang l\u1EA1i cho l\u1EADp tr\xEC\
  nh vi\xEAn m\u1ED9t si\xEAu n\u0103ng l\u1EF1c \u0111\u1EC3 t\u1EA1m d\u1EEBng m\xE3\
  \ c\u1EE7a h\u1ECD, ki\u1EC3m tra c\xE1c bi\u1EBFn v\xE0 b\u01B0\u1EDBc qua m\xE3\
  \ c\u1EE7a h\u1ECD t\u1EEBng d\xF2ng m\u1ED9t.\u2026"
title: "S\u1EED d\u1EE5ng b\u1ED9 g\u1EE1 l\u1ED7i"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Sử dụng debugger trong Ruby mang lại cho lập trình viên một siêu năng lực để tạm dừng mã của họ, kiểm tra các biến và bước qua mã của họ từng dòng một. Mọi người làm điều này để xóa bỏ lỗi, hiểu luồng mã và để xem chính xác điều gì đang xảy ra với mã (phép thuật) mà họ đã viết khi ma thuật được thực hiện - hoặc không.

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
