---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:01.692251-07:00
description: "Ph\xE2n t\xEDch c\xFA ph\xE1p ng\xE0y t\u1EE9c l\xE0 d\u1ECBch m\u1ED9\
  t chu\u1ED7i sang c\xE1i m\xE0 m\xE1y t\xEDnh c\xF3 th\u1EC3 hi\u1EC3u\u2014nh\u01B0\
  \ m\u1ED9t \u0111\u1ED1i t\u01B0\u1EE3ng ng\xE0y th\u1EF1c s\u1EF1. C\xE1c l\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m vi\u1EC7c n\xE0y v\xEC ch\xFAng t\xF4i\u2026"
lastmod: '2024-02-25T18:49:35.688349-07:00'
model: gpt-4-0125-preview
summary: "Ph\xE2n t\xEDch c\xFA ph\xE1p ng\xE0y t\u1EE9c l\xE0 d\u1ECBch m\u1ED9t\
  \ chu\u1ED7i sang c\xE1i m\xE0 m\xE1y t\xEDnh c\xF3 th\u1EC3 hi\u1EC3u\u2014nh\u01B0\
  \ m\u1ED9t \u0111\u1ED1i t\u01B0\u1EE3ng ng\xE0y th\u1EF1c s\u1EF1. C\xE1c l\u1EAD\
  p tr\xECnh vi\xEAn l\xE0m vi\u1EC7c n\xE0y v\xEC ch\xFAng t\xF4i\u2026"
title: "Ph\xE2n t\xEDch ng\xE0y t\u1EEB chu\u1ED7i k\xED t\u1EF1"
---

{{< edit_this_page >}}

## Cái gì và Tại sao?

Phân tích cú pháp ngày tức là dịch một chuỗi sang cái mà máy tính có thể hiểu—như một đối tượng ngày thực sự. Các lập trình viên làm việc này vì chúng tôi thường thu được ngày dưới dạng chuỗi từ các nguồn như form, file, hoặc web, và chúng tôi cần xử lý chúng theo cách có cấu trúc, đáng tin cậy.

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
