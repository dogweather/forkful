---
title:                "Chuyển đổi một ngày thành chuỗi"
aliases:
- /vi/ruby/converting-a-date-into-a-string.md
date:                  2024-01-28T21:58:16.030021-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi một ngày thành chuỗi"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/ruby/converting-a-date-into-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Chuyển đổi một ngày thành một chuỗi chuyển đổi đối tượng ngày thành văn bản mà chúng ta có thể đọc và sử dụng. Các lập trình viên làm điều này để hiển thị ngày theo định dạng thân thiện với con người hoặc chuẩn bị dữ liệu cho việc lưu trữ và giao tiếp, như trong các tệp JSON hoặc CSV.

## Làm thế nào:
Ruby giúp việc vận dụng với ngày và chuỗi trở nên cực kỳ dễ dàng. Dưới đây là cách bạn làm:

```Ruby
require 'date'

# Hãy tạo một đối tượng ngày
my_date = Date.new(2023, 4, 14)

# Chuyển đổi mặc định sang chuỗi
date_string = my_date.to_s
puts date_string  # Kết quả: "2023-04-14"

# Định dạng tùy chỉnh sử dụng strftime (định dạng thời gian chuỗi)
pretty_date = my_date.strftime('%B %d, %Y')
puts pretty_date  # Kết quả: "April 14, 2023"

# Một ví dụ khác, chỉ để thử nghiệm
fun_date_format = my_date.strftime('%d-%m-%Y')
puts fun_date_format  # Kết quả: "14-04-2023"
```

## Sâu hơn nữa
Ngày xưa, mọi người viết ngày bằng tay. Trong thế giới lập trình, lớp `Date` của Ruby đã mang đến cho chúng ta sức mạnh để xử lý ngày mà không cần phải vất vả. Bạn có các phương thức như `to_s` và `strftime` để biến đối tượng `Date` của mình thành chuỗi.

Phương thức `to_s` cung cấp cho bạn một biểu diễn ISO 8601 nhanh chóng (`YYYY-MM-DD`), rất tuyệt vời cho việc chuyển đổi không cần trang trí. Nhưng khi bạn cần ngày của mình "khoác áo đẹp", `strftime` cho phép bạn chọn chính xác mô hình mà chuỗi của mình sẽ theo. Các ký hiệu trong `strftime` như `%Y` cho năm bốn chữ số, `%m` cho tháng hai chữ số, và `%d` cho ngày hai chữ số là những khối xây dựng của bạn để định dạng ngày.

Mặc dù các lớp `Date` và `Time` của Ruby là vững chắc, nhưng các gem như `Timecop` cho việc du hành thời gian (không phải du hành thời gian thực, xin lỗi) trong quá trình kiểm tra, hoặc `Chronic` để phân tích ngày tháng bằng ngôn ngữ tự nhiên, có thể thêm một số ưu điểm khi bạn cần.

Bản chất của nó là gì? Ruby sử dụng các thư viện hệ thống—như các phần giữ thời gian của thư viện C—phía sau hậu trường. Nghĩa là nó nhanh chóng và đáng tin cậy, xử lý các tình huống như năm nhuận và giờ tiết kiệm ánh sáng như một nhà vô địch.

## Xem Thêm
Hãy xem các tài nguyên này để biết thêm chi tiết:
- Tài liệu lớp `Date` của Ruby: [ruby-doc.org/stdlib-2.7.3/libdoc/date/rdoc/Date.html](https://ruby-doc.org/stdlib-3.1.2/libdoc/date/rdoc/Date.html)
- Các chỉ thị `strftime` của Ruby: [apidock.com/ruby/DateTime/strftime](https://apidock.com/ruby/DateTime/strftime)
- Các gem cho thêm phép màu ngày/giờ: [github.com/travisjeffery/timecop](https://github.com/travisjeffery/timecop) và [github.com/mojombo/chronic](https://github.com/mojombo/chronic)
