---
title:                "Sắp xếp mã thành các hàm"
date:                  2024-01-28T22:03:36.785606-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sắp xếp mã thành các hàm"

category:             "Ruby"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
