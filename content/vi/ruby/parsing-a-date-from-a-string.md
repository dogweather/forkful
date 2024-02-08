---
title:                "Phân tích ngày từ chuỗi kí tự"
date:                  2024-01-28T22:05:01.692251-07:00
model:                 gpt-4-0125-preview
simple_title:         "Phân tích ngày từ chuỗi kí tự"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
