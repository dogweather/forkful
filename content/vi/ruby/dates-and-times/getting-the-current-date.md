---
aliases:
- /vi/ruby/getting-the-current-date/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:44.584999-07:00
description: "Vi\u1EC7c l\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i trong Ruby c\u1EF1c k\u1EF3\
  \ \u0111\u01A1n gi\u1EA3n: l\u1EA5y ng\xE0y c\u1EE7a h\xF4m nay. C\xE1c l\u1EAD\
  p tr\xECnh vi\xEAn c\u1EA7n n\xF3 cho nhi\u1EC1u t\xE1c v\u1EE5 kh\xE1c nhau t\u1EEB\
  \ ghi log v\xE0 \u0111\xE1nh d\u1EA5u th\u1EDDi\u2026"
lastmod: 2024-02-18 23:08:51.297219
model: gpt-4-0125-preview
summary: "Vi\u1EC7c l\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i trong Ruby c\u1EF1c k\u1EF3\
  \ \u0111\u01A1n gi\u1EA3n: l\u1EA5y ng\xE0y c\u1EE7a h\xF4m nay. C\xE1c l\u1EAD\
  p tr\xECnh vi\xEAn c\u1EA7n n\xF3 cho nhi\u1EC1u t\xE1c v\u1EE5 kh\xE1c nhau t\u1EEB\
  \ ghi log v\xE0 \u0111\xE1nh d\u1EA5u th\u1EDDi\u2026"
title: "L\u1EA5y ng\xE0y hi\u1EC7n t\u1EA1i"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Việc lấy ngày hiện tại trong Ruby cực kỳ đơn giản: lấy ngày của hôm nay. Các lập trình viên cần nó cho nhiều tác vụ khác nhau từ ghi log và đánh dấu thời gian cho tới lập lịch và kiểm tra tính hợp lệ.

## Làm thế nào:
Ruby giúp việc lấy ngày hiện tại trở nên rất dễ dàng. Dưới đây là cách làm:

```ruby
require 'date'

# Lấy ngày hiện tại
current_date = Date.today
puts current_date
```

Khi chạy đoạn mã này, nó sẽ in ra cái gì đó như thế này (tùy thuộc vào ngày bạn chạy nó):

```
2023-04-07
```

Bạn muốn có cả thời gian nữa? Dưới đây là mã nguồn:

```ruby
require 'time'

# Lấy ngày và giờ hiện tại
current_datetime = Time.now
puts current_datetime
```

Và kết quả đầu ra sẽ bao gồm cả dấu thời gian:

```
2023-04-07 12:34:56 +0900
```

## Sâu hơn nữa
Ngày xưa, các Rubyist cần đến các thư viện bên ngoài để quản lý ngày và giờ. Nhập cuộc thư viện chuẩn của Ruby với các lớp `Date` và `Time`, và nhu cầu cho những thứ phụ trợ chủ yếu đã thành quá khứ.

`Date` xử lý, ừ, ngày - ngày, tháng và năm. Để chính xác hơn, `DateTime` kết hợp ngày và giờ, nhưng nếu bạn chỉ cần giờ hoặc chi tiết cụ thể hơn như giây hoặc múi giờ, `Time` sẽ giúp bạn.

Những lựa chọn thay thế cho các lớp built-in của Ruby bao gồm các gems như 'timecop' để kiểm thử mã phụ thuộc vào thời gian, và 'chronic' để phân tích ngày từ ngôn ngữ tự nhiên.

Về cơ bản, `Date.today` kéo ngày từ hệ thống của bạn. Nó giữ mọi thứ đơn giản nhưng bỏ qua múi giờ. `Time.now` đi xa hơn, tính toán múi giờ với một độ lệch mặc định từ Coordinated Universal Time (UTC).

## Xem thêm
* Tài liệu của Ruby về lớp Time: [https://ruby-doc.org/core-2.7.0/Time.html](https://ruby-doc.org/core-2.7.0/Time.html)
* Gem 'timecop' để giả lập với thời gian: [https://github.com/travisjeffery/timecop](https://github.com/travisjeffery/timecop)
* Gem 'chronic' để phân tích ngày bằng ngôn ngữ tự nhiên: [https://github.com/mojombo/chronic](https://github.com/mojombo/chronic)
