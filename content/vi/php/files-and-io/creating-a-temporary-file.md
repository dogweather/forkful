---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:49.112476-07:00
description: "L\xE0m Th\u1EBF N\xE0o: PHP gi\xFAp b\u1EA1n t\u1EA1o t\u1EC7p t\u1EA1\
  m th\u1EDDi v\u1EDBi h\xE0m `tmpfile()`, h\xE0m n\xE0y t\u1EA1o m\u1ED9t t\u1EC7\
  p cho b\u1EA1n trong th\u01B0 m\u1EE5c temp c\u1EE7a h\u1EC7 th\u1ED1ng. D\u01B0\
  \u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5 nhanh."
lastmod: '2024-03-13T22:44:36.792750-06:00'
model: gpt-4-0125-preview
summary: "PHP gi\xFAp b\u1EA1n t\u1EA1o t\u1EC7p t\u1EA1m th\u1EDDi v\u1EDBi h\xE0\
  m `tmpfile()`, h\xE0m n\xE0y t\u1EA1o m\u1ED9t t\u1EC7p cho b\u1EA1n trong th\u01B0\
  \ m\u1EE5c temp c\u1EE7a h\u1EC7 th\u1ED1ng."
title: "T\u1EA1o m\u1ED9t t\u1EADp tin t\u1EA1m th\u1EDDi"
weight: 21
---

## Làm Thế Nào:
PHP giúp bạn tạo tệp tạm thời với hàm `tmpfile()`, hàm này tạo một tệp cho bạn trong thư mục temp của hệ thống. Dưới đây là một ví dụ nhanh:

```PHP
<?php
$tempFile = tmpfile();
fwrite($tempFile, "Xin chào, thế giới tạm thời!");
rewind($tempFile);

echo fread($tempFile, 1024); // Đọc những gì chúng ta đã viết vào tệp

fclose($tempFile); // Tệp tạm thời sẽ được xóa tự động
?>
```

Đầu Ra Mẫu:
```
Xin chào, thế giới tạm thời!
```

Bạn cũng có thể sử dụng `tempnam()` để nhận được một tên tệp mà bạn có thể quản lý:

```PHP
<?php
$tempFilePath = tempnam(sys_get_temp_dir(), 'Tux');
file_put_contents($tempFilePath, "Chim cánh cụt thật cool!");

echo file_get_contents($tempFilePath); // Đọc nội dung

unlink($tempFilePath); // Xóa tệp khi bạn đã xong
?>
```

Đầu Ra Mẫu:
```
Chim cánh cụt thật cool!
```

## Sâu Hơn
Hàm `tmpfile()` đã có trong PHP từ những ngày đầu. Nó xử lý việc tạo và dọn dẹp tệp cho bạn, né tránh khéo léo các rủi ro về bảo mật khi để thông tin nhạy cảm tồn tại lâu dài.

Ngược lại, `tempnam()` chỉ cung cấp cho bạn một cái tên, để lại việc quản lý tệp trong tay bạn. Một lưu ý: luôn nhớ `unlink()` tệp khi bạn đã xong.

Những tệp tạm thời này thường được lưu trữ trong thư mục temp mặc định của hệ thống của bạn, mà bạn có thể tìm thấy với `sys_get_temp_dir()`. Vị trí này có thể thay đổi tùy thuộc vào hệ điều hành và cấu hình môi trường của bạn.

Bạn cũng có các lựa chọn thay thế như `tempnam()` và `tmpfile()`, và có `sys_get_temp_dir()` tinh vi hơn để lấy được thư mục temp khó tìm đó. Nhưng nhớ một quy tắc vàng với các tệp tạm thời: dọn dẹp sau khi sử dụng—PHP làm một số việc này tự động, nhưng thực hành rõ ràng luôn là điều tốt.

## Xem Thêm
- [Tài liệu chính thức PHP về hàm tmpfile()](https://www.php.net/manual/en/function.tmpfile.php)
- [Hướng dẫn sử dụng PHP về hàm tempnam()](https://www.php.net/manual/en/function.tempnam.php)
- [Thông tin về sys_get_temp_dir() trên PHP.net](https://www.php.net/manual/en/function.sys-get-temp-dir.php)
- [Bảo mật Hệ thống Tệp](https://www.php.net/manual/en/security.filesystem.php)
