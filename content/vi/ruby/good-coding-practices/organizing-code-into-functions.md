---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:36.785606-07:00
description: "T\u1ED5 ch\u1EE9c m\xE3 l\u1EC7nh th\xE0nh c\xE1c h\xE0m chia nh\u1ECF\
  \ k\u1ECBch b\u1EA3n c\u1EE7a b\u1EA1n th\xE0nh c\xE1c kh\u1ED1i c\xF3 th\u1EC3\
  \ t\xE1i s\u1EED d\u1EE5ng. \u0110i\u1EC1u n\xE0y gi\xFAp cho m\xE3 c\u1EE7a b\u1EA1\
  n tr\u1EDF n\xEAn g\u1ECDn g\xE0ng, d\u1EC5 qu\u1EA3n l\xFD v\xE0 \xEDt\u2026"
lastmod: '2024-03-11T00:14:10.650820-06:00'
model: gpt-4-0125-preview
summary: "T\u1ED5 ch\u1EE9c m\xE3 l\u1EC7nh th\xE0nh c\xE1c h\xE0m chia nh\u1ECF k\u1ECB\
  ch b\u1EA3n c\u1EE7a b\u1EA1n th\xE0nh c\xE1c kh\u1ED1i c\xF3 th\u1EC3 t\xE1i s\u1EED\
  \ d\u1EE5ng. \u0110i\u1EC1u n\xE0y gi\xFAp cho m\xE3 c\u1EE7a b\u1EA1n tr\u1EDF\
  \ n\xEAn g\u1ECDn g\xE0ng, d\u1EC5 qu\u1EA3n l\xFD v\xE0 \xEDt\u2026"
title: "S\u1EAFp x\u1EBFp m\xE3 th\xE0nh c\xE1c h\xE0m"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tổ chức mã lệnh thành các hàm chia nhỏ kịch bản của bạn thành các khối có thể tái sử dụng. Điều này giúp cho mã của bạn trở nên gọn gàng, dễ quản lý và ít lỗi hơn. Mã modular (mã được tổ chức theo mô đun) tuyệt vời vì nó giúp bạn tiết kiệm thời gian, giữ được sự tỉnh táo và làm đơn giản hóa việc gỡ lỗi và kiểm thử đơn vị.

## Làm thế nào:
Hãy tưởng tượng bạn đang viết một kịch bản nhanh để chào mừng người dùng:

```Ruby
def greet(name)
  "Xin chào, #{name}!"
end

puts greet("Alice")   # Kết quả: Xin chào, Alice!
puts greet("Bob")     # Kết quả: Xin chào, Bob!
```

Hoặc có thể bạn đang tính diện tích của một hình tròn:

```Ruby
def circle_area(radius)
  Math::PI * radius ** 2
end

puts circle_area(5)   # Kết quả: 78.53981633974483
```

Gọn gàng và dễ quản lý hơn, phải không?

## Sâu hơn nữa
Khái niệm về hàm, cũng được biết đến là phương thức trong Ruby, không phải là mới—nó cũ như chính lập trình. Quay trở lại những năm 1950, các chương trình con, như chúng được biết đến, đã được giới thiệu để giảm bớt sự trùng lặp.

Có lựa chọn khác sao? Chắc chắn, bạn có mã nội tuyến, bạn có thể đi theo hướng OOP với các lớp và đối tượng, hoặc thậm chí là hướng hàm với lambdas và procs. Nhưng hàm là nền tảng của mã lệnh gọn gàng. Muốn hiệu suất? Biến cục bộ trong hàm nhanh và hàm có thể trả về giá trị ngay lập tức với `return`.

Về mặt triển khai, bạn có thể định nghĩa một hàm với `def` và kết thúc nó với `end`. Bạn có thể thiết lập các tham số mặc định, sử dụng các toán tử splat cho các hàm variadic, và hơn thế nữa. Hàm có thể đơn giản hoặc phức tạp theo ý muốn của bạn.

## Tham khảo thêm
- [Tài liệu về phương thức của Ruby](https://ruby-doc.org/core-2.7.0/Method.html)
- [Học lập trình với Chris Pine](https://pine.fm/LearnToProgram/)
- [Thiết kế Hướng Đối Tượng Thực Tế trong Ruby của Sandi Metz](https://www.poodr.com/)
