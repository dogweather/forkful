---
title:                "Viết một tệp văn bản"
date:                  2024-01-28T22:13:06.504693-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết một tệp văn bản"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/writing-a-text-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc viết một tệp văn bản trong PHP là để lưu dữ liệu vào một tệp trên máy chủ. Các lập trình viên thường làm điều này để ghi nhật ký dữ liệu, cài đặt cấu hình, hoặc xuất dữ liệu dành cho việc đọc bởi con người hoặc các hệ thống khác.

## Làm thế nào:

Việc viết vào một tệp trong PHP có thể đơn giản như sử dụng hàm `file_put_contents()`, đòi hỏi tên tệp và một chuỗi dữ liệu. Dưới đây là một ví dụ nhanh:

```php
<?php
$data = "Xin chào, thế giới!\n";
file_put_contents("example.txt", $data);
?>
```

Khi chạy đoạn mã này, "example.txt" được tạo ra với nội dung "Xin chào, thế giới!".

Để kiểm soát nhiều hơn, bạn có thể mở một tệp, viết vào đó, sau đó đóng nó lại:

```php
<?php
$file = fopen("example.txt", "w") or die("Không thể mở tệp!");
$txt = "Xin chào lại, thế giới!\n";
fwrite($file, $txt);
fclose($file);
?>
```

Cả hai đoạn mã đều tạo ra cùng một kết quả trong "example.txt".

## Tìm hiểu sâu hơn

Trong lịch sử, PHP's `fopen()`, `fwrite()`, và `fclose()` cung cấp kiểm soát chi tiết cho các hoạt động viết tệp, như thêm vào cuối hoặc khóa tệp. `file_put_contents()` được giới thiệu trong PHP 5 cho một cách tiếp cận đơn giản hơn.

Các lựa chọn khác bao gồm sử dụng `fputcsv()` để tạo tệp CSV hoặc lớp `SplFileObject` cho các hoạt động tệp theo hướng đối tượng. Các chi tiết thực hiện bao gồm xử lý quyền tệp và đảm bảo xử lý ngoại lệ hoặc kiểm tra lỗi với `or die()` hoặc khối `try-catch`.

## Xem thêm

- [PHP file_put_contents()](https://www.php.net/manual/en/function.file-put-contents.php)
- [PHP fopen()](https://www.php.net/manual/en/function.fopen.php)
- [PHP fwrite()](https://www.php.net/manual/en/function.fwrite.php)
- [Xử lý tệp PHP](https://www.php.net/manual/en/book.filesystem.php)
- [Hiểu về quyền tệp](https://www.php.net/manual/en/function.chmod.php)
