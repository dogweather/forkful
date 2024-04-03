---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:08:43.450253-07:00
description: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n PHP m\u1EDBi c\xF3 ngh\u0129\
  a l\xE0 thi\u1EBFt l\u1EADp m\u1ED9t n\u1EC1n m\xF3ng \u0111\u1EC3 x\xE2y d\u1EF1\
  ng \u1EE9ng d\u1EE5ng web ho\u1EB7c script c\u1EE7a b\u1EA1n. Ch\xFAng ta l\xE0\
  m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 b\u1EAFt \u0111\u1EA7u t\u1EEB \u0111\u1EA7\
  u, c\u1EA5u\u2026"
lastmod: '2024-03-13T22:44:36.767458-06:00'
model: gpt-4-0125-preview
summary: "B\u1EAFt \u0111\u1EA7u m\u1ED9t d\u1EF1 \xE1n PHP m\u1EDBi c\xF3 ngh\u0129\
  a l\xE0 thi\u1EBFt l\u1EADp m\u1ED9t n\u1EC1n m\xF3ng \u0111\u1EC3 x\xE2y d\u1EF1\
  ng \u1EE9ng d\u1EE5ng web ho\u1EB7c script c\u1EE7a b\u1EA1n."
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
