---
title:                "Chuyển đổi chuỗi thành chữ thường"
aliases:
- /vi/php/converting-a-string-to-lower-case/
date:                  2024-01-28T21:58:25.320789-07:00
model:                 gpt-4-0125-preview
simple_title:         "Chuyển đổi chuỗi thành chữ thường"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Trong PHP, việc chuyển đổi một chuỗi thành chữ thường có nghĩa là biến đổi tất cả các ký tự chữ cái trong chuỗi thành biến thể chữ thường của chúng. Các lập trình viên làm điều này để đạt được sự nhất quán, đặc biệt là khi so sánh hoặc sắp xếp các chuỗi, nơi mà sự khác biệt về chữ hoa chữ thường có thể gây rối loạn.

## Làm thế nào:

PHP sử dụng `strtolower` để chuyển tất cả các ký tự trong một chuỗi thành chữ thường. Dưới đây là cách nó hoạt động:

```php
<?php
$chuoiGoc = "HeLLo WoRLD!";
$chuoiChuThuong = strtolower($chuoiGoc);

echo $chuoiChuThuong; // Xuất ra: hello world!
?>
```

Nếu bạn cần xử lý mã hóa ký tự đa byte, như UTF-8, hãy sử dụng `mb_strtolower` thay thế:

```php
<?php
$chuoiGoc = "İstanbul";
$chuoiChuThuong = mb_strtolower($chuoiGoc, 'UTF-8');

echo $chuoiChuThuong; // Xuất ra: istanbul (chuyển đổi đúng từ İ thành i)
?>
```

## Tìm hiểu sâu hơn

Về lịch sử, hàm `strtolower` của PHP đã là hàm đi đến cho việc chuyển đổi chữ hoa thành chữ thường, được giới thiệu trong các phiên bản rất sơ khai của PHP. Tuy nhiên, khi các ứng dụng PHP trở nên phổ biến toàn cầu, nhu cầu xử lý chính xác mã hóa ký tự đa byte đã dẫn đến `mb_strtolower`.

Các phương án thay thế cho `strtolower` và `mb_strtolower` bao gồm sử dụng biểu thức chính quy với hàm `mb_ereg_replace_callback` hoặc `preg_replace_callback`, nhưng cho việc chuyển đổi trường hợp đơn giản, chúng là quá mức.

Trong PHP, chuỗi truyền thống đã được dựa trên byte, không phải dựa trên ký tự, có nghĩa là mỗi byte là một ký tự. Điều này hoạt động cho mã hóa ký tự đơn byte như ASCII, nơi mỗi ký tự đúng là một byte. Đối với mã hóa đa byte, `mb_strtolower` hiểu mã hóa ký tự và xử lý các ký tự như chúng nên được xử lý.

## Xem thêm

- Hướng dẫn PHP về `strtolower`: https://www.php.net/manual/en/function.strtolower.php
- Hướng dẫn PHP về `mb_strtolower`: https://www.php.net/manual/en/function.mb-strtolower.php
- UTF-8 và Unicode cho các nhà phát triển PHP: https://www.php.net/manual/en/book.mbstring.php
