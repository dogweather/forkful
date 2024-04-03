---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:59.747038-07:00
description: "L\xE0m th\u1EBF n\xE0o: B\u1EA1n c\xF3 th\u1EC3 ghi v\xE0o `stderr`\
  \ trong PHP v\u1EDBi `fwrite()` ho\u1EB7c b\u1ED9 \u0111i\u1EC1u khi\u1EC3n lu\u1ED3\
  ng. D\u01B0\u1EDBi \u0111\xE2y l\xE0 c\xE1ch l\xE0m."
lastmod: '2024-03-13T22:44:36.788349-06:00'
model: gpt-4-0125-preview
summary: "B\u1EA1n c\xF3 th\u1EC3 ghi v\xE0o `stderr` trong PHP v\u1EDBi `fwrite()`\
  \ ho\u1EB7c b\u1ED9 \u0111i\u1EC1u khi\u1EC3n lu\u1ED3ng."
title: "Ghi v\xE0o l\u1ED7i chu\u1EA9n"
weight: 25
---

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
