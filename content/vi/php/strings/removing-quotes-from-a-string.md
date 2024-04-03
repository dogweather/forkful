---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:42.544642-07:00
description: "L\xE0m th\u1EBF n\xE0o: D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xED\
  \ d\u1EE5 \u0111\u01A1n gi\u1EA3n s\u1EED d\u1EE5ng c\xE1c h\xE0m c\xF3 s\u1EB5\
  n trong PHP."
lastmod: '2024-03-13T22:44:36.750125-06:00'
model: gpt-4-0125-preview
summary: "D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5 \u0111\u01A1n gi\u1EA3\
  n s\u1EED d\u1EE5ng c\xE1c h\xE0m c\xF3 s\u1EB5n trong PHP."
title: "Lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c k\xE9p kh\u1ECFi chu\u1ED7i"
weight: 9
---

## Làm thế nào:
Dưới đây là một ví dụ đơn giản sử dụng các hàm có sẵn trong PHP:

```php
$quotedString = "'Hello,' she said, \"It's a fine day!\"";
$unquotedString = str_replace(array("'", "\""), '', $quotedString);
echo $unquotedString; // Đầu ra: Hello, she said, Its a fine day!
```

Đơn giản, phải không? Hàm `str_replace()` này nhận một mảng các ký tự để loại bỏ khỏi chuỗi, bao gồm cả dấu ngoặc đơn và dấu ngoặc kép.

## Sâu hơn
Ngay từ những ngày đầu của PHP, các nhà phát triển phải cực kỳ cẩn thận với dấu ngoặc trong chuỗi, đặc biệt là khi chèn dữ liệu vào cơ sở dữ liệu. Dấu ngoặc không được xử lý đúng cách có thể dẫn đến các cuộc tấn công SQL injection. Nhập cuộc dấu ngoặc ma thuật, một tính năng tự động thoát dữ liệu đầu vào. Nó đã bị loại bỏ và cuối cùng bị gỡ bỏ vì khuyến khích thực hành lập trình không tốt và các vấn đề an ninh.

Bây giờ, chúng ta sử dụng các hàm như `str_replace()` hoặc regex với `preg_replace()` cho các mẫu phức tạp hơn. Dưới đây là một ví dụ regex:

```php
$quotedString = "'Hello,' she said, \"It's a fine day!\"";
$unquotedString = preg_replace('/[\'"]/', '', $quotedString);
echo $unquotedString;
```

Đối với dữ liệu JSON, bạn có thể sử dụng `json_encode()` với các tùy chọn như `JSON_UNESCAPED_SLASHES | JSON_UNESCAPED_UNICODE` để tránh các dấu gạch chéo thêm trong dấu ngoặc của bạn.

Khi triển khai, hãy xem xét các trường hợp ngoại lệ. Nếu chuỗi của bạn có ý định chứa một số dấu ngoặc nhất định, như lời thoại trong câu chuyện hoặc đo lường bằng inch? Ngữ cảnh quan trọng, vì vậy hãy tùy chỉnh việc loại bỏ dấu ngoặc của bạn theo mục đích sử dụng dữ liệu.

## Xem Thêm
- [PHP: str_replace](https://www.php.net/manual/en/function.str-replace.php)
- [PHP: preg_replace](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP: json_encode](https://www.php.net/manual/en/function.json-encode.php)
- [OWASP: Phòng chống SQL Injection](https://owasp.org/www-community/attacks/SQL_Injection)
