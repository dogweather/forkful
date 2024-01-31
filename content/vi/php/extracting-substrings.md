---
title:                "Trích xuất chuỗi con"
date:                  2024-01-28T22:00:11.609208-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trích xuất chuỗi con"

category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
