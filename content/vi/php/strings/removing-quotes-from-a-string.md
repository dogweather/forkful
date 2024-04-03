---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:42.544642-07:00
description: "Lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c kh\u1ECFi m\u1ED9t chu\u1ED7i\
  \ trong PHP c\xF3 ngh\u0129a l\xE0 lo\u1EA1i b\u1ECF nh\u1EEFng k\xFD t\u1EF1 d\u1EA5\
  u ngo\u1EB7c k\xE9p (`\"`) ho\u1EB7c d\u1EA5u ngo\u1EB7c \u0111\u01A1n (`'`) c\xF3\
  \ th\u1EC3 l\xE0m r\u1ED1i lo\u1EA1n logic code\u2026"
lastmod: '2024-03-13T22:44:36.750125-06:00'
model: gpt-4-0125-preview
summary: "Lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c kh\u1ECFi m\u1ED9t chu\u1ED7i trong\
  \ PHP c\xF3 ngh\u0129a l\xE0 lo\u1EA1i b\u1ECF nh\u1EEFng k\xFD t\u1EF1 d\u1EA5\
  u ngo\u1EB7c k\xE9p (`\"`) ho\u1EB7c d\u1EA5u ngo\u1EB7c \u0111\u01A1n (`'`) c\xF3\
  \ th\u1EC3 l\xE0m r\u1ED1i lo\u1EA1n logic code ho\u1EB7c truy v\u1EA5n c\u01A1\
  \ s\u1EDF d\u1EEF li\u1EC7u c\u1EE7a b\u1EA1n."
title: "Lo\u1EA1i b\u1ECF d\u1EA5u ngo\u1EB7c k\xE9p kh\u1ECFi chu\u1ED7i"
weight: 9
---

## Cái gì & Tại sao?
Loại bỏ dấu ngoặc khỏi một chuỗi trong PHP có nghĩa là loại bỏ những ký tự dấu ngoặc kép (`"`) hoặc dấu ngoặc đơn (`'`) có thể làm rối loạn logic code hoặc truy vấn cơ sở dữ liệu của bạn. Lập trình viên làm điều này để làm sạch hoặc khử trùng dữ liệu đầu vào, đảm bảo rằng chuỗi được sử dụng hoặc lưu trữ một cách an toàn.

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
