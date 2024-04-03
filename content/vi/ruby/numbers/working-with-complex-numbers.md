---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:12:50.221261-07:00
description: "L\xE0m th\u1EBF n\xE0o: Ruby l\xE0m cho vi\u1EC7c x\u1EED l\xFD s\u1ED1\
  \ ph\u1EE9c tr\u1EDF n\xEAn d\u1EC5 d\xE0ng. B\u1EA1n c\xF3 th\u1EC3 t\u1EA1o v\xE0\
  \ thao t\xE1c v\u1EDBi ch\xFAng s\u1EED d\u1EE5ng l\u1EDBp Complex."
lastmod: '2024-03-13T22:44:37.329134-06:00'
model: gpt-4-0125-preview
summary: "Ruby l\xE0m cho vi\u1EC7c x\u1EED l\xFD s\u1ED1 ph\u1EE9c tr\u1EDF n\xEA\
  n d\u1EC5 d\xE0ng."
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
weight: 14
---

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
