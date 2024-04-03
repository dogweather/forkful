---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:01.692251-07:00
description: "L\xE0m th\u1EBF n\xE0o: Ruby \u0111\xE3 h\u1ED7 tr\u1EE3 b\u1EA1n v\u1EDB\
  i th\u01B0 vi\u1EC7n `Date` c\u1EE7a m\xECnh, gi\xFAp bi\u1EBFn \u0111\u1ED5i chu\u1ED7\
  i th\xE0nh ng\xE0y c\u1EF1c k\u1EF3 d\u1EC5 d\xE0ng. Ch\u1EC9 \u0111\u1EEBng qu\xEA\
  n `require 'date'` tr\u01B0\u1EDBc khi b\u1EA1n\u2026"
lastmod: '2024-03-13T22:44:37.353766-06:00'
model: gpt-4-0125-preview
summary: "Ruby \u0111\xE3 h\u1ED7 tr\u1EE3 b\u1EA1n v\u1EDBi th\u01B0 vi\u1EC7n `Date`\
  \ c\u1EE7a m\xECnh, gi\xFAp bi\u1EBFn \u0111\u1ED5i chu\u1ED7i th\xE0nh ng\xE0y\
  \ c\u1EF1c k\u1EF3 d\u1EC5 d\xE0ng."
title: "Ph\xE2n t\xEDch ng\xE0y t\u1EEB chu\u1ED7i k\xED t\u1EF1"
weight: 30
---

## Làm thế nào:
Ruby đã hỗ trợ bạn với thư viện `Date` của mình, giúp biến đổi chuỗi thành ngày cực kỳ dễ dàng. Chỉ đừng quên `require 'date'` trước khi bạn bắt đầu.

```ruby
require 'date'

# Phân tích cú pháp một ngày (định dạng ISO) từ chuỗi
date_string = "2023-04-01"
parsed_date = Date.parse(date_string)
puts parsed_date
# => 2023-04-01

# Nếu định dạng của chúng ta đi lạc hướng thì sao? Hãy thử điều này.
begin
  custom_date = Date.strptime('03/31/2023', '%m/%d/%Y')
  puts custom_date
rescue ArgumentError
  puts "Đó không phải là định dạng ngày hợp lệ, bạn à."
end
# => 2023-03-31
```

## Tìm hiểu sâu hơn
Trong quá khứ, Ruby ít khoan dung hơn đối với các định dạng ngày. Các lập trình viên phải thủ công vật lộn với chuỗi để trích xuất ngày. Bây giờ, `Date.parse` tự động phát hiện hầu hết các định dạng ngày phổ biến, và nếu nó bị nhầm lẫn, `Date.strptime` cho phép bạn chỉ định chính xác định dạng để tránh hiểu nhầm.

Ngoài ra, nếu bạn đang xử lý dữ liệu ngày-giờ phức tạp hơn, `DateTime` có thể là lựa chọn của bạn, đặc biệt là để phân tích cả thời gian. Hơn nữa, đối với những người sử dụng ngoài thư viện chuẩn của Ruby, có gem `Chronic` do những người thông minh ở GitHub tạo ra, hiểu một loạt các biểu thức ngày bằng ngôn ngữ tự nhiên.

Phía dưới tất cả sự đơn giản này, việc phân tích cú pháp của Ruby thực sự được hỗ trợ bởi các mẫu định dạng ngày phù hợp với các phần khác nhau của chuỗi ngày với các yếu tố ngày tương ứng (năm, tháng, ngày, v.v.). Vì vậy, khi chuỗi của bạn không khớp với mẫu mong đợi, bạn sẽ cần phải thông báo cho Ruby với `strptime` và các hướng dẫn định dạng đúng.

## Xem thêm
- Đối với những người hâm mộ múi giờ, tài liệu `ActiveSupport::TimeWithZone` trong Rails có thể thú vị: [https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html](https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html)
- Gem Chronic cho phân tích cú pháp ngày bằng ngôn ngữ tự nhiên: [https://github.com/mojombo/chronic](https://github.com/mojombo/chronic)
