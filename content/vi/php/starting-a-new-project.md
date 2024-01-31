---
title:                "Bắt đầu một dự án mới"
date:                  2024-01-28T22:08:43.450253-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bắt đầu một dự án mới"

category:             "PHP"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/starting-a-new-project.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Điều gì và Tại sao?
Bắt đầu một dự án PHP mới có nghĩa là thiết lập một nền móng để xây dựng ứng dụng web hoặc script của bạn. Chúng ta làm điều này để bắt đầu từ đầu, cấu trúc hóa ý tưởng của chúng ta vào mã lệnh và giải quyết vấn đề mới.

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
