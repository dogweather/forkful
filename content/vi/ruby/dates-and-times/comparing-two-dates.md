---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:19.890693-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Ruby l\xE0m cho cu\u1ED9c s\u1ED1ng c\u1EE7\
  a ch\xFAng ta tr\u1EDF n\xEAn \u0111\u01A1n gi\u1EA3n h\u01A1n v\u1EDBi l\u1EDB\
  p Date. H\xE3y xem n\xF3 ho\u1EA1t \u0111\u1ED9ng nh\u01B0 th\u1EBF n\xE0o."
lastmod: '2024-03-13T22:44:37.357896-06:00'
model: gpt-4-0125-preview
summary: "Ruby l\xE0m cho cu\u1ED9c s\u1ED1ng c\u1EE7a ch\xFAng ta tr\u1EDF n\xEA\
  n \u0111\u01A1n gi\u1EA3n h\u01A1n v\u1EDBi l\u1EDBp Date."
title: "So s\xE1nh hai ng\xE0y"
weight: 27
---

## Cách thực hiện:
Ruby làm cho cuộc sống của chúng ta trở nên đơn giản hơn với lớp Date. Hãy xem nó hoạt động như thế nào.

```ruby
require 'date'

date1 = Date.new(2023, 3, 14)
date2 = Date.new(2023, 3, 15)

puts date1 == date2   # Đầu ra: false
puts date1 != date2   # Đầu ra: true
puts date1 < date2    # Đầu ra: true
puts date1 > date2    # Đầu ra: false
puts date1 <= Date.today # Đầu ra: tùy thuộc vào ngày hôm nay
puts date1 >= Date.today # Đầu ra: tùy thuộc vào ngày hôm nay
```

## Sâu hơn
So sánh ngày không phải là điều mới. Nó cơ bản như so sánh số nguyên, nhưng khó hơn vì ngày có các phần—ngày, tháng, năm. Trong Ruby, lớp Date (từ thư viện chuẩn) gánh vác, xử lý với tháng, năm nhuận, v.v.

Bạn đã thấy các so sánh cơ bản: `==`, `!=`, `<`, `>`, `<=`, `>=`. Nhưng Ruby còn nhiều điều hơn nữa ẩn bên dưới.

* `Date.parse` có thể hiểu và chuyển đổi ngày từ chuỗi.
* `DateTime` cung cấp độ chính xác cao hơn, với hỗ trợ thời gian và múi giờ.
* Thư viện như 'ActiveSupport' (từ Rails) thêm nhiều phương thức liên quan đến ngày nữa.

Cẩn thận với bẫy:
* Múi giờ có thể khiến bạn mắc lỗi nếu bạn không cẩn thận.
* Giây nhuận không được tính trong các lớp Date/DateTime chuẩn của Ruby.

Các phương án thay thế cho lớp Date bao gồm:

* Sử dụng timestamp và so sánh chúng như số.
* Thư viện 'time' cho việc xử lý thời gian nâng cao hơn.

So sánh trở nên phức tạp nhanh chóng. Nếu bạn đang lên lịch và cần so sánh các phạm vi ngày, hoặc xử lý các sự kiện định kỳ? Các trừu tượng hóa cấp cao hơn được xây dựng trên Date và Time của Ruby thường là cần thiết. Phương thức `between?` của ActiveRecord hoặc các gem như 'IceCube' cho các sự kiện định kỳ có thể tiết kiệm rất nhiều thời gian và tránh được nhức đầu.

## Xem thêm
- Mở rộng của ActiveSupport: [Active Support Core Extensions](https://edgeguides.rubyonrails.org/active_support_core_extensions.html)
- Gem 'IceCube' cho các sự kiện định kỳ: [IceCube](https://github.com/seejohnrun/ice_cube)
- Hướng dẫn toàn diện về múi giờ trong Ruby: [Hướng dẫn về múi giờ](https://thoughtbot.com/blog/its-about-time-zones)
