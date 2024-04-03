---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:34.712060-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: PHP \u0111i k\xE8m v\u1EDBi m\u1ED9t ch\u1EE9\
  c n\u0103ng ghi log l\u1ED7i t\xEDch h\u1EE3p s\u1EB5n r\u1EA5t d\u1EC5 s\u1EED\
  \ d\u1EE5ng. Ch\u1EC9 c\u1EA7n th\xEAm `error_log()` v\xE0o m\xE3 c\u1EE7a b\u1EA1\
  n \u0111\u1EC3 g\u1EEDi m\u1ED9t th\xF4ng b\xE1o \u0111\u1EBFn\u2026"
lastmod: '2024-03-13T22:44:36.775627-06:00'
model: gpt-4-0125-preview
summary: "PHP \u0111i k\xE8m v\u1EDBi m\u1ED9t ch\u1EE9c n\u0103ng ghi log l\u1ED7\
  i t\xEDch h\u1EE3p s\u1EB5n r\u1EA5t d\u1EC5 s\u1EED d\u1EE5ng."
title: Ghi log
weight: 17
---

## Cách thực hiện:
PHP đi kèm với một chức năng ghi log lỗi tích hợp sẵn rất dễ sử dụng. Chỉ cần thêm `error_log()` vào mã của bạn để gửi một thông báo đến log của máy chủ. Bạn cũng có thể tùy chỉnh nó để ghi vào một tệp cụ thể.

```php
<?php
// Ghi một thông báo thông tin đơn giản
error_log("Đây là một mục nhập log thông tin.");

// Ghi một thông báo lỗi
error_log("Đây là một mục nhập log lỗi.", 0);

// Ghi vào một tệp được chỉ định
file_put_contents('/path/to/your/custom.log', "Một mục nhập log tùy chỉnh.\n", FILE_APPEND);

// Sử dụng Monolog cho ghi log có cấu trúc
require 'vendor/autoload.php';
use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// Tạo logger
$logger = new Logger('tên');
// Bây giờ thêm vào một số trình xử lý
$logger->pushHandler(new StreamHandler('/path/to/your/monolog.log', Logger::WARNING));

// Bạn có thể sử dụng logger của mình
$logger->warning('Đây là một log cảnh báo!');
$logger->error('Đây là một log lỗi!');
?>
```

Điều này sẽ xuất ra các log của bạn vào log của máy chủ hoặc tệp bạn chỉ định ở dạng văn bản thuần túy.

## Sâu Hơn:
Trong lịch sử, các nhà phát triển PHP dựa vào chức năng `error_log()` hoặc log của Apache/Nginx để nắm bắt các vấn đề, nhưng điều đó có thể trở nên hỗn loạn với nhu cầu phải phân tích các tệp văn bản thuần túy và không có cách dễ dàng để lọc hoặc sắp xếp chúng. Thế là các thư viện ghi log như Monolog xuất hiện, mở đầu cho kỷ nguyên của ghi log có cấu trúc trong PHP. Những giải pháp này mang lại cho bạn sự kiểm soát tốt hơn bằng cách cung cấp nhiều kênh ghi log, mức độ nghiêm trọng và định dạng đầu ra được định dạng (như JSON, là giấc mơ cho việc phân tích một cách lập trình).

Các phương án thay thế cho Monolog bao gồm Log4php, KLogger và Log4php của Apache. Về mặt triển khai, ghi log mạnh mẽ đòi hỏi không chỉ đơn giản là đổ dữ liệu mọi nơi, mà còn phải xem xét các yếu tố như xoay log, chiến lược lưu trữ và tích hợp với công cụ giám sát để thực sự hữu ích.

Bạn nên giữ [Giao Diện Logger PSR-3](https://www.php-fig.org/psr/psr-3/) trong tâm trí, mô tả một giao diện chung cho các thư viện ghi log, đảm bảo khả năng tương thích và cách tiếp cận nhất quán để truy cập cơ chế ghi log.

## Xem Thêm:
- [Kho lưu trữ GitHub của Monolog](https://github.com/Seldaek/monolog)
- [Thông số kỹ thuật Giao Diện Logger PSR-3](https://www.php-fig.org/psr/psr-3/)
- [Tài liệu PHP Error Log](https://www.php.net/manual/en/function.error-log.php)
- [KLogger: Một Lớp Ghi Log Đơn Giản Cho PHP](https://github.com/katzgrau/KLogger)
- [Log4php: Một khuôn khổ ghi log linh hoạt cho PHP](https://logging.apache.org/log4php/)

Hãy bắt đầu với các chức năng tích hợp sẵn, nhưng để có một cách tiếp cận bảo trì và mở rộng hơn, hãy cân nhắc dành thời gian để làm quen với một thư viện như Monolog. Chúc bạn ghi log vui vẻ!
