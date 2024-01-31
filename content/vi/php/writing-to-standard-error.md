---
title:                "Ghi vào lỗi chuẩn"
date:                  2024-01-28T22:13:59.747038-07:00
model:                 gpt-4-0125-preview
simple_title:         "Ghi vào lỗi chuẩn"

category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/writing-to-standard-error.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Việc ghi vào lỗi chuẩn (`stderr`) có nghĩa là xuất thông điệp lỗi và chẩn đoán riêng biệt với đầu ra chuẩn (`stdout`). Các lập trình viên thực hiện điều này để gỡ lỗi và cung cấp thông tin lỗi mà không lẫn lộn với đầu ra chương trình thông thường.

## Làm thế nào:

Bạn có thể ghi vào `stderr` trong PHP với `fwrite()` hoặc bộ điều khiển luồng. Dưới đây là cách làm:

```PHP
<?php
// Ghi vào stderr với fwrite
fwrite(STDERR, "Đây là thông điệp lỗi.\n");

// Sử dụng bộ điều khiển luồng
file_put_contents('php://stderr', "Đây là một thông điệp lỗi khác.\n");
?>
```

Đầu ra mẫu (trong bảng điều khiển):
```
Đây là thông điệp lỗi.
Đây là một thông điệp lỗi khác.
```

## Sâu hơn nữa

Lịch sử, việc tách biệt `stdout` và `stderr` xuất phát từ cách Unix xử lý luồng vào/ra. Các ngôn ngữ khác như C cũng có quy ước tương tự. Các phương án thay thế trong PHP có thể bao gồm các thư viện log hoặc trình xử lý lỗi tùy chỉnh, nhưng việc viết trực tiếp vào `stderr` là phương pháp đơn giản cho các ứng dụng console. Đằng sau hậu trường, `stderr` là một luồng đầu ra không được đệm, có nghĩa là các thông điệp được xả ngay lập tức mà không chờ đợi.

## Xem thêm

- Hướng dẫn PHP về Hằng Số Định Nghĩa Sẵn (STDERR): https://www.php.net/manual/en/features.commandline.io-streams.php
- Hướng dẫn PHP về các chức năng xử lý lỗi: https://www.php.net/manual/en/book.errorfunc.php
- Wikipedia về Luồng chuẩn: https://en.wikipedia.org/wiki/Standard_streams
