---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:50.221261-07:00
description: "S\u1ED1 ph\u1EE9c, g\u1ED3m c\xF3 m\u1ED9t ph\u1EA7n th\u1EF1c v\xE0\
  \ m\u1ED9t ph\u1EA7n \u1EA3o (nh\u01B0 3+4i), l\xE0 n\u1EC1n t\u1EA3ng trong k\u1EF9\
  \ thu\u1EADt v\xE0 v\u1EADt l\xFD. L\u1EADp tr\xECnh vi\xEAn l\xE0m vi\u1EC7c v\u1EDB\
  i ch\xFAng trong c\xE1c m\xF4 ph\u1ECFng, x\u1EED l\xFD\u2026"
lastmod: '2024-03-13T22:44:37.329134-06:00'
model: gpt-4-0125-preview
summary: "S\u1ED1 ph\u1EE9c, g\u1ED3m c\xF3 m\u1ED9t ph\u1EA7n th\u1EF1c v\xE0 m\u1ED9\
  t ph\u1EA7n \u1EA3o (nh\u01B0 3+4i), l\xE0 n\u1EC1n t\u1EA3ng trong k\u1EF9 thu\u1EAD\
  t v\xE0 v\u1EADt l\xFD. L\u1EADp tr\xECnh vi\xEAn l\xE0m vi\u1EC7c v\u1EDBi ch\xFA\
  ng trong c\xE1c m\xF4 ph\u1ECFng, x\u1EED l\xFD\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
weight: 14
---

## Cái gì và Tại sao?
Số phức, gồm có một phần thực và một phần ảo (như 3+4i), là nền tảng trong kỹ thuật và vật lý. Lập trình viên làm việc với chúng trong các mô phỏng, xử lý tín hiệu và giải các phương trình không thể giải quyết chỉ với số thực.

## Làm thế nào:
Ruby làm cho việc xử lý số phức trở nên dễ dàng. Bạn có thể tạo và thao tác với chúng sử dụng lớp Complex:

```ruby
require 'complex'

# Tạo số phức
c1 = Complex(3, 4)
c2 = Complex('2+5i')

# Các phép toán cơ bản
tong = c1 + c2               # => (5.0+9.0i)
hieu = c1 - c2        # => (1.0-1.0i)
tich = c1 * c2           # => (-14.0+23.0i)
thuong = c1 / c2          # => (0.896551724137931+0.03448275862068961i)

# Liên hợp, độ lớn và pha
lien_hop = c1.conjugate    # => (3.0-4.0i)
do_lon = c1.abs           # => 5.0
pha = c1.phase            # Math.atan2(4, 3) => 0.9272952180016122 radian

# Các phương thức đặc biệt cho số phức
polar = c1.polar            # => [5.0, 0.9272952180016122]
hinh_chu_nhat = c1.rect       # => [3.0, 4.0]
```

## Sâu hơn
Số phức không phải là mới—they've được sử dụng từ thế kỷ 16, giải các phương trình không có nghiệm thực. Để lại mặt toán học, về mặt tính toán, lớp Complex của Ruby làm phần nặng nhọc, được hỗ trợ bởi mô-đun Math cho các hàm lượng giác và siêu việt.

Những ngôn ngữ lập trình trước đây yêu cầu xử lý thủ công phần thực và ảo. Một số, như Fortran và C++, dành riêng các thư viện đặc biệt cho toán học phức.

Cách tiếp cận của Ruby nhúng hỗ trợ số phức vào cú pháp của nó, giải bạn khỏi việc phát minh lại cái bánh xe. Đằng sau hậu trường, lớp Complex xử lý toán học, trong khi Ruby chăm sóc các tương tác đối tượng.

## Xem thêm
- Tài liệu Ruby về Complex: [https://ruby-doc.org/core/Complex.html](https://ruby-doc.org/core/Complex.html)
- Quan điểm của MathWorld về Số Phức: [http://mathworld.wolfram.com/ComplexNumber.html](http://mathworld.wolfram.com/ComplexNumber.html)
- Giới thiệu trực quan về số phức và lý do chúng hữu ích: [https://www.youtube.com/watch?v=5PcpBw5Hbwo](https://www.youtube.com/watch?v=5PcpBw5Hbwo)
