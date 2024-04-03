---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:06.504693-07:00
description: "L\xE0m th\u1EBF n\xE0o: Vi\u1EC7c vi\u1EBFt v\xE0o m\u1ED9t t\u1EC7\
  p trong PHP c\xF3 th\u1EC3 \u0111\u01A1n gi\u1EA3n nh\u01B0 s\u1EED d\u1EE5ng h\xE0\
  m `file_put_contents()`, \u0111\xF2i h\u1ECFi t\xEAn t\u1EC7p v\xE0 m\u1ED9t chu\u1ED7\
  i d\u1EEF li\u1EC7u. D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t\u2026"
lastmod: '2024-03-13T22:44:36.791077-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c vi\u1EBFt v\xE0o m\u1ED9t t\u1EC7p trong PHP c\xF3 th\u1EC3 \u0111\
  \u01A1n gi\u1EA3n nh\u01B0 s\u1EED d\u1EE5ng h\xE0m `file_put_contents()`, \u0111\
  \xF2i h\u1ECFi t\xEAn t\u1EC7p v\xE0 m\u1ED9t chu\u1ED7i d\u1EEF li\u1EC7u."
title: "Vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n"
weight: 24
---

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
