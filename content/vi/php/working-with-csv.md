---
title:                "Làm việc với CSV"
aliases:
- vi/php/working-with-csv.md
date:                  2024-01-28T22:11:30.179834-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/working-with-csv.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
