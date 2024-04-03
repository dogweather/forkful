---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:06.504693-07:00
description: "Vi\u1EC7c vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n trong PHP l\xE0\
  \ \u0111\u1EC3 l\u01B0u d\u1EEF li\u1EC7u v\xE0o m\u1ED9t t\u1EC7p tr\xEAn m\xE1\
  y ch\u1EE7. C\xE1c l\u1EADp tr\xECnh vi\xEAn th\u01B0\u1EDDng l\xE0m \u0111i\u1EC1\
  u n\xE0y \u0111\u1EC3 ghi nh\u1EADt k\xFD d\u1EEF li\u1EC7u, c\xE0i \u0111\u1EB7\
  t c\u1EA5u\u2026"
lastmod: '2024-03-13T22:44:36.791077-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c vi\u1EBFt m\u1ED9t t\u1EC7p v\u0103n b\u1EA3n trong PHP l\xE0\
  \ \u0111\u1EC3 l\u01B0u d\u1EEF li\u1EC7u v\xE0o m\u1ED9t t\u1EC7p tr\xEAn m\xE1\
  y ch\u1EE7."
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
