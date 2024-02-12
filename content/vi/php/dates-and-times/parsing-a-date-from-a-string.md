---
title:                "Phân tích ngày từ chuỗi kí tự"
date:                  2024-01-28T22:04:42.220981-07:00
model:                 gpt-4-0125-preview
simple_title:         "Phân tích ngày từ chuỗi kí tự"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/parsing-a-date-from-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Phân tích cú pháp một ngày từ một chuỗi nghĩa là chuyển đổi văn bản đại diện cho ngày và giờ thành một định dạng có thể lập trình được. Lập trình viên làm điều này để thao tác với ngày tháng, so sánh chúng, hoặc lưu trữ chúng vào cơ sở dữ liệu một cách hiệu quả.

## Làm thế nào:

PHP làm cho việc phân tích cú pháp ngày từ chuỗi khá đơn giản với lớp `DateTime`. Dưới đây là một ví dụ nhanh:

```php
<?php
$dateString = '2023-04-12 14:00:00';
$dateTime = new DateTime($dateString);

echo $dateTime->format('Y-m-d H:i:s'); // Đầu ra: 2023-04-12 14:00:00
?>
```

Đơn giản, phải không? Bây giờ, muốn thay đổi định dạng hoặc múi giờ? Dưới đây là cách làm:

```php
<?php
$dateString = 'April 12, 2023 14:00:00';
$dateTime = DateTime::createFromFormat('F j, Y H:i:s', $dateString);
$dateTime->setTimezone(new DateTimeZone('Europe/London'));

echo $dateTime->format('Y-m-d H:i'); // Đầu ra: 2023-04-12 14:00
?>
```

Thử nghiệm với các định dạng và các múi giờ để thấy sự mạnh mẽ của nó.

## Tìm hiểu sâu

Trong quá khứ, các nhà phát triển PHP phải tự mình phân tích cú pháp chuỗi ngày hoặc sử dụng `strtotime()`, cái mà hiệu quả nhưng kém mạnh mẽ hơn `DateTime`. Được giới thiệu trong PHP 5.2.0, `DateTime` cung cấp việc thao tác ngày/giờ theo hướng đối tượng.

Tại sao thay đổi? Bởi vì `DateTime`:

1. Xử lý ngoại lệ.
2. Làm việc với các lịch khác nhau.
3. Nhận biết múi giờ.
4. Có nhiều tùy chọn định dạng và phân tích cú pháp hơn.

Các lựa chọn khác bao gồm `IntlDateFormatter` cho việc quốc tế hóa hoặc thư viện `Carbon` cho cú pháp đường viền hiện đại.

Khi phân tích, cẩn thận với các cạm bẫy:

- Luôn kiểm tra đầu vào. Định dạng không chính xác gây ra ngày sai hoặc lỗi.
- Múi giờ quan trọng. Lưu trữ ở UTC và hiển thị địa phương.
- Giây nhuận và giờ tiết kiệm ban ngày có thể ảnh hưởng đến việc tính toán.

## Xem thêm

- [Hướng dẫn PHP về DateTime](https://www.php.net/manual/en/class.datetime.php)
- [Các hàm ngày và giờ PHP](https://www.php.net/manual/en/ref.datetime.php)
- [Carbon: Một phần mở rộng API PHP đơn giản cho DateTime](https://carbon.nesbot.com/)
