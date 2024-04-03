---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:27.853144-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong PHP, h\xE0m `date()` \u0111\u1ECB\
  nh d\u1EA1ng m\u1ED9t d\u1EA5u th\u1EDDi gian th\xE0nh chu\u1ED7i d\u1EC5 \u0111\
  \u1ECDc h\u01A1n. \u0110\u1ED1i t\u01B0\u1EE3ng `DateTime` ph\u1EE5c v\u1EE5 m\u1EE5\
  c \u0111\xEDch t\u01B0\u01A1ng t\u1EF1 v\u1EDBi ph\u01B0\u01A1ng th\u1EE9c\u2026"
lastmod: '2024-03-13T22:44:36.782067-06:00'
model: gpt-4-0125-preview
summary: "Trong PHP, h\xE0m `date()` \u0111\u1ECBnh d\u1EA1ng m\u1ED9t d\u1EA5u th\u1EDD\
  i gian th\xE0nh chu\u1ED7i d\u1EC5 \u0111\u1ECDc h\u01A1n."
title: "Chuy\u1EC3n \u0111\u1ED5i m\u1ED9t ng\xE0y th\xE0nh chu\u1ED7i"
weight: 28
---

## Cách thực hiện:
Trong PHP, hàm `date()` định dạng một dấu thời gian thành chuỗi dễ đọc hơn. Đối tượng `DateTime` phục vụ mục đích tương tự với phương thức `format()` của nó. Dưới đây là cách chúng được sử dụng trong thực tế:

```php
<?php
// Sử dụng hàm date()
echo date('Y-m-d H:i:s') . "\n"; // kết quả: 2023-04-03 14:30:00 (ví dụ)

// Sử dụng đối tượng DateTime
$dateTime = new DateTime();
echo $dateTime->format('Y-m-d H:i:s') . "\n"; // kết quả: giống như trên
?>
```
Kết quả mẫu phản ánh ngày và thời gian khi mã được chạy.

## Sâu hơn
Trong lịch sử, PHP đã phát triển trong việc xử lý ngày và giờ. Các phiên bản PHP đầu tiên có ít tính năng thao tác ngày hơn. Lớp `DateTime`, được giới thiệu trong PHP 5.2.0, cung cấp cách xử lý hướng đối tượng, hỗ trợ múi giờ, và nhiều tính linh hoạt hơn.

Các lựa chọn thay thế cho `date()` và `DateTime` bao gồm:
- `strftime()` (định dạng nhận thức về địa phương)
- `DateTimeImmutable` (phiên bản bất biến của `DateTime`)
- Các lớp mở rộng như `Carbon` cho những nhu cầu phức tạp hơn

Nội bộ, cả `date()` và `DateTime` đều phụ thuộc vào cài đặt múi giờ của máy chủ trừ khi được chỉ định khác. Lớp `DateTimeZone` có thể thao tác múi giờ.

## Xem thêm
- [Hướng dẫn PHP: Chức năng Ngày và Giờ](https://www.php.net/manual/en/book.datetime.php)
- [PHP Đúng Cách: Ngày và Thời gian](https://phptherightway.com/#date_and_time)
- [Carbon: Một tiện ích mở rộng PHP đơn giản cho DateTime](https://carbon.nesbot.com/)
