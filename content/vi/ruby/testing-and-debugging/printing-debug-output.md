---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:37.304676-07:00
description: "In \u0111\u1EA7u ra \u0111\u1EC3 g\u1EE1 l\u1ED7i trong Ruby gi\u1ED1\
  ng nh\u01B0 vi\u1EC7c \u0111\u1EC3 l\u1EA1i d\u1EA5u v\u1EBFt trong m\xE3 c\u1EE7\
  a b\u1EA1n \u0111\u1EC3 theo d\xF5i gi\xE1 tr\u1ECB c\u1EE7a bi\u1EBFn v\xE0 d\xF2\
  ng ch\u1EA3y c\u1EE7a ch\u01B0\u01A1ng tr\xECnh. C\xE1c l\u1EADp tr\xECnh\u2026"
lastmod: '2024-03-13T22:44:37.344083-06:00'
model: gpt-4-0125-preview
summary: "In \u0111\u1EA7u ra \u0111\u1EC3 g\u1EE1 l\u1ED7i trong Ruby gi\u1ED1ng\
  \ nh\u01B0 vi\u1EC7c \u0111\u1EC3 l\u1EA1i d\u1EA5u v\u1EBFt trong m\xE3 c\u1EE7\
  a b\u1EA1n \u0111\u1EC3 theo d\xF5i gi\xE1 tr\u1ECB c\u1EE7a bi\u1EBFn v\xE0 d\xF2\
  ng ch\u1EA3y c\u1EE7a ch\u01B0\u01A1ng tr\xECnh."
title: "In ra th\xF4ng tin g\u1EE1 l\u1ED7i"
weight: 33
---

## Cách thực hiện:
Trong Ruby, `puts` và `p` là các phương thức bạn nên sử dụng khi muốn xuất đầu ra nhanh chóng lên console.

```Ruby
def who_said_what
  quote = "To be or not to be"
  author = "Shakespeare"
  puts "Quote: #{quote}"
  p "Said by: #{author}"
end

who_said_what
```

Đầu ra mẫu:

```
Quote: To be or not to be
"Said by: Shakespeare"
```

Phương thức `puts` in ra đầu ra dễ đọc cho con người, thêm một dòng mới ở cuối. Ngược lại, `p` in giá trị một cách thô sơ hơn, hữu ích khi bạn cần xem một cái gì đó có phải là chuỗi hay không.

## Sâu hơn
Trước đây, khi chưa có các IDE tiên tiến, việc in ra console chính là việc gỡ lỗi. Đây là một kỹ thuật cũ nhưng vàng, đặc biệt khi bạn muốn tránh tổn thất thời gian thiết lập một trình gỡ lỗi.

Là các phương án thay thế, bạn có thể sử dụng `pp` cho việc in đẹp các đối tượng phức tạp, hoặc các thư viện gem như `awesome_print` để tăng cường khả năng đọc. Nếu đầu ra gỡ lỗi của bạn trở nên quá nhiều, hãy cân nhắc sử dụng một thư viện log để kiểm soát mức độ chi tiết.

Về mặt triển khai, `puts` và `p` viết vào `$stdout`, một dòng I/O toàn cục trong Ruby. Đầu ra có thể được chuyển hướng nếu cần. Nhớ rằng, mặc dù những phương thức này tiện lợi, nhưng việc in quá nhiều có thể làm lộn xộn console và làm việc gỡ lỗi trở nên khó khăn hơn.

## Xem thêm
- Tài liệu Ruby cho `Kernel#puts`: https://ruby-doc.org/core/Kernel.html#method-i-puts
- Tài liệu Ruby cho `Kernel#p`: https://ruby-doc.org/core/Kernel.html#method-i-p
- Hướng dẫn về in đẹp trong Ruby: https://ruby-doc.org/stdlib/libdoc/pp/rdoc/PP.html
- Gem Awesome Print cho đầu ra ấn tượng: https://rubygems.org/gems/awesome_print/
