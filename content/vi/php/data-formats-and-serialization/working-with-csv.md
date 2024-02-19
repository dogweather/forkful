---
aliases:
- /vi/php/working-with-csv/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:30.179834-07:00
description: "CSV, vi\u1EBFt t\u1EAFt c\u1EE7a Comma-Separated Values (Gi\xE1 tr\u1ECB\
  \ \u0111\u01B0\u1EE3c ph\xE2n t\xE1ch b\u1EDFi d\u1EA5u ph\u1EA9y), l\xE0 m\u1ED9\
  t \u0111\u1ECBnh d\u1EA1ng t\u1EC7p ph\u1ED5 bi\u1EBFn \u0111\u1EC3 l\u01B0u tr\u1EEF\
  \ d\u1EEF li\u1EC7u b\u1EA3ng. L\u1EADp tr\xECnh vi\xEAn s\u1EED\u2026"
lastmod: 2024-02-18 23:08:50.815211
model: gpt-4-0125-preview
summary: "CSV, vi\u1EBFt t\u1EAFt c\u1EE7a Comma-Separated Values (Gi\xE1 tr\u1ECB\
  \ \u0111\u01B0\u1EE3c ph\xE2n t\xE1ch b\u1EDFi d\u1EA5u ph\u1EA9y), l\xE0 m\u1ED9\
  t \u0111\u1ECBnh d\u1EA1ng t\u1EC7p ph\u1ED5 bi\u1EBFn \u0111\u1EC3 l\u01B0u tr\u1EEF\
  \ d\u1EEF li\u1EC7u b\u1EA3ng. L\u1EADp tr\xECnh vi\xEAn s\u1EED\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi CSV"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
CSV, viết tắt của Comma-Separated Values (Giá trị được phân tách bởi dấu phẩy), là một định dạng tệp phổ biến để lưu trữ dữ liệu bảng. Lập trình viên sử dụng nó bởi vì nó đơn giản, được hỗ trợ rộng rãi, và có thể dễ dàng đọc và viết bởi cả máy tính và con người.

## Cách thực hiện:

### Đọc một tệp CSV
```php
<?php
$filename = 'data.csv';

if (($handle = fopen($filename, "r")) !== FALSE) {
    while (($data = fgetcsv($handle, 1000, ",")) !== FALSE) {
        echo "Hàng: " . print_r($data, true) . "\n";
    }
    fclose($handle);
}
?>
```
Dữ liệu đầu ra mẫu:
```
Hàng: Mảng
(
    [0] => Tên
    [1] => Tuổi
    [2] => Email
)

Hàng: Mảng
(
    [0] => John Doe
    [1] => 30
    [2] => john@example.com
)
```

### Viết vào một tệp CSV
```php
<?php
$list = [
  ['Tên', 'Tuổi', 'Email'],
  ['Jane Doe', '25', 'jane@example.com'],
  ['John Smith', '40', 'john.smith@example.com']
];

$filename = 'output.csv';

$handle = fopen($filename, 'w');

foreach ($list as $fields) {
    fputcsv($handle, $fields);
}

fclose($handle);
?>
```

## Tìm hiểu sâu hơn
CSV đã xuất hiện từ những ngày đầu của ngành công nghiệp máy tính, khiến nó trở thành một trong những định dạng lưu trữ dữ liệu bền bỉ nhất. Mặc dù JSON và XML cung cấp nhiều tính phức tạp hơn, CSV vẫn phổ biến vì sự đơn giản của nó. Khi sử dụng PHP để thao tác với các tệp CSV, bạn tương tác với hệ thống tệp thông qua các hàm tích hợp sẵn như `fgetcsv()` và `fputcsv()`. Những hàm này đóng gói các chi tiết nhỏ nhặt của việc phân tích và viết tệp, khiến quá trình trở nên khá đơn giản. Lưu ý rằng hàm `fgetcsv()` cho phép bạn xác định một tham số độ dài và một dấu phân cách, mà bạn có thể cần điều chỉnh theo đặc điểm cụ thể của tệp CSV của mình.

## Xem thêm
- Tài liệu chính thức của PHP về fgetcsv: https://www.php.net/manual/en/function.fgetcsv.php
- Tài liệu chính thức của PHP về fputcsv: https://www.php.net/manual/en/function.fputcsv.php
- Giới thiệu về xử lý CSV với PHP: https://www.php.net/manual/en/book.fileinfo.php
- Trình chỉnh sửa và kiểm tra CSV trực tuyến: https://csvlint.io/
- RFC 4180, Định dạng Chung và Kiểu MIME cho các tệp Comma-Separated Values (CSV): https://tools.ietf.org/html/rfc4180
