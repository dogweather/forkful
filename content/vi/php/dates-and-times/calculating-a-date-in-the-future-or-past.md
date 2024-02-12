---
title:                "Tính toán ngày trong tương lai hoặc quá khứ"
aliases: - /vi/php/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-28T21:56:05.941217-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tính toán ngày trong tương lai hoặc quá khứ"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái Gì và Tại Sao?
Tính toán ngày trong tương lai hoặc quá khứ có nghĩa là tìm kiếm một ngày trước hoặc sau một thời điểm cụ thể. Các lập trình viên làm điều này cho các chức năng như nhắc nhở, đăng ký, lập lịch, và hàng tấn các tính năng dựa trên thời gian khác trong ứng dụng.

## Làm Thế Nào:
PHP làm cho phép tính ngày trở nên đơn giản với `DateTime` và `DateInterval`. Hãy xem xét điều này:

```PHP
<?php
// Ngày hôm nay
$today = new DateTime();
echo $today->format('Y-m-d H:i:s') . "\n";

// Thêm 10 ngày
$today->add(new DateInterval('P10D'));
echo $today->format('Y-m-d H:i:s') . "\n";

// Trừ đi 2 tháng
$today->sub(new DateInterval('P2M'));
echo $today->format('Y-m-d H:i:s') . "\n";
?>
```
Kết quả có thể là:
```
2023-04-01 12:34:56
2023-04-11 12:34:56
2023-02-11 12:34:56
```

## Tìm Hiểu Sâu Hơn
Ngày xưa, các phép tính ngày trong PHP thường xuyên xảy ra lỗi hơn. `strtotime`, mặc dù vẫn hữu ích, nhưng có thể gây bối rối với các trường hợp biên. `DateTime` và `DateInterval` mang lại độ chính xác và sự rõ ràng theo hướng đối tượng.

Có phương án thay thế không? Chắc chắn rồi. Các thư viện như Carbon bọc chức năng ngày tháng của PHP để tăng tính dễ đọc và tính năng, nhưng trong nhiều trường hợp, các lớp có sẵn của PHP là hoàn toàn ổn.

Bên trong, `DateTime::add()` và `DateTime::sub()` thay đổi đối tượng, nên không cần phải gán lại. Chúng xử lý các đơn vị thời gian một cách nhất quán, tính đến những vấn đề như năm nhuận và sự thay đổi giờ tiết kiệm ánh sáng, điều này có thể gây đau đầu nếu không.

## Xem Thêm
- Hướng dẫn sử dụng PHP về DateTime: https://www.php.net/manual/en/class.datetime.php
- Tài liệu về DateInterval: https://www.php.net/manual/en/class.dateinterval.php
- Carbon: Một API mở rộng đơn giản cho DateTime - https://carbon.nesbot.com
