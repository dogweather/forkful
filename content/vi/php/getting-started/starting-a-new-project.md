---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:43.450253-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Kh\u1EDFi \u0111\u1ED9ng d\u1EF1 \xE1n\
  \ c\u1EE7a b\u1EA1n b\u1EB1ng c\xE1ch ch\u1ECDn m\u1ED9t c\u1EA5u tr\xFAc. Composer\
  \ l\xE0 b\u1EA1n c\u1EE7a b\u1EA1n \u1EDF \u0111\xE2y. Ch\u1EA1y l\u1EC7nh n\xE0\
  y."
lastmod: '2024-03-13T22:44:36.767458-06:00'
model: gpt-4-0125-preview
summary: "Kh\u1EDFi \u0111\u1ED9ng d\u1EF1 \xE1n c\u1EE7a b\u1EA1n b\u1EB1ng c\xE1\
  ch ch\u1ECDn m\u1ED9t c\u1EA5u tr\xFAc."
title: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n m\u1EDBi"
weight: 1
---

## Cách thực hiện:
Khởi động dự án của bạn bằng cách chọn một cấu trúc. Composer là bạn của bạn ở đây. Chạy lệnh này:

```bash
composer init
```

Sau đó, tạo `index.php` của bạn. Đây là điểm nhập của bạn:

```php
<?php
// index.php
echo "Chào mừng đến với dự án PHP mới của tôi!";
```

Kiểm tra nó trên máy chủ địa phương của bạn. Bạn sẽ thấy:

```
Chào mừng đến với dự án PHP mới của tôi!
```

## Đào sâu
Vào thời điểm đó, các dự án PHP bắt đầu với các script đơn giản. Không quản lý gói, không có frameworks, chỉ có các tệp PHP thuần túy. Bây giờ, chúng ta có Composer, một công cụ để quản lý các phụ thuộc, tự động nạp các lớp, và thiết lập các tiêu chuẩn tự động nạp như PSR-4. Đó là thực hành tiêu chuẩn cho các dự án PHP hiện đại.

Bạn có thể thực hiện theo phong cách cũ, không Composer, không tự động nạp. Nhưng tại sao lại bỏ qua sự tiện lợi và các tiêu chuẩn được chấp nhận rộng rãi?

Sử dụng các frameworks như Laravel hoặc Symfony cho các ứng dụng phức tạp. Chúng cung cấp cấu trúc và công cụ, tăng tốc quá trình phát triển. Đối với các dự án nhỏ hơn, các micro-framework như Slim có thể đủ.

Về mặt triển khai, hãy xem xét sử dụng biến môi trường cho cấu hình, áp dụng các tiêu chuẩn PSR cho phong cách và cấu trúc mã, và không bỏ qua hệ thống kiểm soát phiên bản như Git.

## Xem thêm
- [Composer](https://getcomposer.org/)
- [PHP Cách Đúng](https://phptherightway.com/)
- [Tiêu chuẩn PSR](https://www.php-fig.org/psr/)
- [Laravel](https://laravel.com/)
- [Symfony](https://symfony.com/)
- [Khung làm việc Slim](http://www.slimframework.com/)
