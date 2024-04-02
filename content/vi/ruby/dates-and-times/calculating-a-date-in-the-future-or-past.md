---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:00.183070-07:00
description: "T\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1 kh\u1EE9\
  \ c\xF3 ngh\u0129a l\xE0 t\xECm hi\u1EC3u ng\xE0y s\u1EBD l\xE0 g\xEC, ho\u1EB7\
  c \u0111\xE3 l\xE0 g\xEC, sau ho\u1EB7c tr\u01B0\u1EDBc m\u1ED9t kho\u1EA3ng th\u1EDD\
  i gian nh\u1EA5t \u0111\u1ECBnh. L\u1EADp tr\xECnh\u2026"
lastmod: '2024-03-13T22:44:37.359175-06:00'
model: gpt-4-0125-preview
summary: "T\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1 kh\u1EE9\
  \ c\xF3 ngh\u0129a l\xE0 t\xECm hi\u1EC3u ng\xE0y s\u1EBD l\xE0 g\xEC, ho\u1EB7\
  c \u0111\xE3 l\xE0 g\xEC, sau ho\u1EB7c tr\u01B0\u1EDBc m\u1ED9t kho\u1EA3ng th\u1EDD\
  i gian nh\u1EA5t \u0111\u1ECBnh. L\u1EADp tr\xECnh\u2026"
title: "T\xEDnh to\xE1n ng\xE0y trong t\u01B0\u01A1ng lai ho\u1EB7c qu\xE1 kh\u1EE9"
weight: 26
---

## Cái gì & Tại sao?

Tính toán ngày trong tương lai hoặc quá khứ có nghĩa là tìm hiểu ngày sẽ là gì, hoặc đã là gì, sau hoặc trước một khoảng thời gian nhất định. Lập trình viên làm điều này cho các tính năng như nhắc nhở, đăng ký, hoặc phân tích dữ liệu lịch sử.

## Làm thế nào:

Ruby làm việc với ngày cực kỳ đơn giản thông qua lớp `Date` đã tích hợp sẵn và gem `active_support` cho một số tính năng thú vị. Dưới đây là cách thực hiện:

```Ruby
require 'date'
require 'active_support/core_ext/integer'

# Lấy ngày hôm nay
today = Date.today
puts "Hôm nay là: #{today}"

# Tính toán ngày 10 ngày trong tương lai
future_date = today + 10
puts "10 ngày nữa sẽ là: #{future_date}"

# Tính toán ngày 30 ngày trong quá khứ
past_date = today - 30
puts "30 ngày trước là: #{past_date}"

# Các tính toán phức tạp hơn với active_support
puts "Trong 2 tháng nữa, sẽ là: #{2.months.from_now.to_date}"
puts "100 ngày trước, là: #{100.days.ago.to_date}"
```

Đầu ra mẫu:

```
Hôm nay là: 2023-04-07
10 ngày nữa sẽ là: 2023-04-17
30 ngày trước là: 2023-03-08
Trong 2 tháng nữa, sẽ là: 2023-06-07
100 ngày trước, là: 2022-12-28
```

## Tìm hiểu kỹ hơn

Trước khi Ruby tích hợp các chức năng tính toán ngày vào các thư viện chuẩn và bổ sung của mình, nhà phát triển thường phải tính toán ngày một cách thủ công, xem xét năm nhuận, chiều dài khác nhau của các tháng, và múi giờ—điều này khá là đau đầu.

Lớp `Date` chuẩn làm được rất nhiều thứ ngay từ hộp. Bạn có thể cộng (`+`) hoặc trừ (`-`) ngày một cách dễ dàng. Tuy nhiên, để thao tác khoảng thời gian một cách trực quan hơn, như "2 tháng từ bây giờ", chúng ta dựa vào `active_support`, được rút ra từ Ruby on Rails. Gem này sử dụng các tiện ích mở rộng cho các lớp Ruby chuẩn, làm cho các tính toán này thân thiện hơn với con người.

Khi tính toán ngày trong quá khứ hoặc tương lai, nếu bạn cũng xem xét vào thời gian (`DateTime` hoặc `Time`), hãy xem xét đến múi giờ. Lớp `Time` của Ruby và `active_support` có thể xử lý điều này nhưng cần một chút cài đặt thêm.

Còn tồn tại các lựa chọn khác, như các gem `time-lord` và `ice_cube`, cung cấp thêm đường viền ngữ pháp hoặc các tính năng đặc biệt (như sự kiện định kỳ), tương ứng.

## Tham khảo

- Xử lý múi giờ trong Ruby: [https://api.rubyonrails.org/classes/ActiveSupport/TimeZone.html](https://api.rubyonrails.org/classes/ActiveSupport/TimeZone.html)
- Gem `time-lord` cho các biểu thức giống như con người: [https://github.com/krainboltgreene/time-lord](https://github.com/krainboltgreene/time-lord)
- Gem `ice_cube` cho việc xử lý các sự kiện định kỳ: [https://github.com/seejohnrun/ice_cube](https://github.com/seejohnrun/ice_cube)
