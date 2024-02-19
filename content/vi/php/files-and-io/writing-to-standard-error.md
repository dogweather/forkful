---
aliases:
- /vi/php/writing-to-standard-error/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:59.747038-07:00
description: "Vi\u1EC7c ghi v\xE0o l\u1ED7i chu\u1EA9n (`stderr`) c\xF3 ngh\u0129\
  a l\xE0 xu\u1EA5t th\xF4ng \u0111i\u1EC7p l\u1ED7i v\xE0 ch\u1EA9n \u0111o\xE1n\
  \ ri\xEAng bi\u1EC7t v\u1EDBi \u0111\u1EA7u ra chu\u1EA9n (`stdout`). C\xE1c l\u1EAD\
  p tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u\u2026"
lastmod: 2024-02-18 23:08:50.809062
model: gpt-4-0125-preview
summary: "Vi\u1EC7c ghi v\xE0o l\u1ED7i chu\u1EA9n (`stderr`) c\xF3 ngh\u0129a l\xE0\
  \ xu\u1EA5t th\xF4ng \u0111i\u1EC7p l\u1ED7i v\xE0 ch\u1EA9n \u0111o\xE1n ri\xEA\
  ng bi\u1EC7t v\u1EDBi \u0111\u1EA7u ra chu\u1EA9n (`stdout`). C\xE1c l\u1EADp tr\xEC\
  nh vi\xEAn th\u1EF1c hi\u1EC7n \u0111i\u1EC1u\u2026"
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
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
