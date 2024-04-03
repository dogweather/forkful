---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:11.609208-07:00
description: "Vi\u1EC7c tr\xEDch xu\u1EA5t c\xE1c chu\u1ED7i con l\xE0 vi\u1EC7c l\u1EA5\
  y ra nh\u1EEFng ph\u1EA7n c\u1EE5 th\u1EC3 t\u1EEB m\u1ED9t chu\u1ED7i. L\u1EAD\
  p tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3 x\u1EED l\xFD\
  \ ho\u1EB7c thao t\xE1c d\u1EEF li\u1EC7u, nh\u01B0 l\u1EA5y t\xEAn\u2026"
lastmod: '2024-03-13T22:44:36.751551-06:00'
model: gpt-4-0125-preview
summary: "Vi\u1EC7c tr\xEDch xu\u1EA5t c\xE1c chu\u1ED7i con l\xE0 vi\u1EC7c l\u1EA5\
  y ra nh\u1EEFng ph\u1EA7n c\u1EE5 th\u1EC3 t\u1EEB m\u1ED9t chu\u1ED7i."
title: "Tr\xEDch xu\u1EA5t chu\u1ED7i con"
weight: 6
---

## Cái gì & Tại sao?
Việc trích xuất các chuỗi con là việc lấy ra những phần cụ thể từ một chuỗi. Lập trình viên thực hiện việc này để xử lý hoặc thao tác dữ liệu, như lấy tên người dùng từ địa chỉ email hay một phần mở rộng tệp từ tên file.

## Làm thế nào:
PHP cung cấp một số hàm để trích xuất các chuỗi con. Hãy xem xét `substr`, `mb_substr`, và `strstr`.

```PHP
$string = "Hello, World! Programming is fun.";

// Trích ‘World’ sử dụng substr.
echo substr($string, 7, 5); // Đầu ra: World

// Ví dụ về chuỗi UTF-8 với mb_substr cho các ký tự đa byte.
$utf8String = "こんにちは世界";
echo mb_substr($utf8String, 5, 2); // Đầu ra: 世

// Lấy mọi thứ sau dấu phẩy với strstr.
echo strstr($string, ","); // Đầu ra: , World! Programming is fun.
```

## Tìm Hiểu Sâu
Trong những ngày đầu của PHP, cách chính để lấy một phần của chuỗi là `substr()`. Tuy nhiên, `substr()` có (và vẫn có) một hạn chế: nó không hoạt động tốt với các ký tự không phải tiếng Anh (như tiếng Nhật hoặc tiếng Ả Rập).

Nhập `mb_substr()`, phiên bản an toàn với nhiều byte hơn, tôn trọng các ký tự từ nhiều mã hoá khác nhau. Nó đảm bảo rằng khi bạn kéo một chuỗi con, bạn không đang xé qua một ký tự giữa byte, điều này rất quan trọng cho các ứng dụng quốc tế.

`strstr()`, ngược lại, tìm kiếm lần xuất hiện đầu tiên của một chuỗi con và cung cấp cho bạn mọi thứ sau nó. Cũng có `strchr()` là bí danh của `strstr()`.

Trong khi `substr()` và `mb_substr()` cho phép bạn chỉ định chính xác nơi bắt đầu và bao nhiêu để lấy, `strstr()` giống như một công cụ "tìm và cho tôi phần còn lại".

## Xem Thêm
Đây là một số tài liệu đọc thêm nếu bạn muốn tìm hiểu thêm:

- Tài liệu chính thức của PHP cho các hàm chuỗi: https://www.php.net/manual/en/ref.strings.php
- Tìm hiểu sâu về các hàm chuỗi đa byte của PHP: https://www.php.net/manual/en/book.mbstring.php
- Thêm thông tin về mã hoá ký tự và tại sao nó quan trọng: http://kunststube.net/encoding/
