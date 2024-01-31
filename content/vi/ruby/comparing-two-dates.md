---
title:                "So sánh hai ngày"
date:                  2024-01-28T21:57:19.890693-07:00
model:                 gpt-4-0125-preview
simple_title:         "So sánh hai ngày"

category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/comparing-two-dates.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

So sánh hai ngày tức là kiểm tra xem chúng có bằng nhau không, hoặc tìm hiểu xem cái nào đến trước hoặc sau cái kia. Lập trình viên làm điều này để theo dõi sự kiện, xử lý đặt chỗ, sắp xếp dòng thời gian, và bất kỳ nhiệm vụ nào mà thứ tự thời gian có ý nghĩa.

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
