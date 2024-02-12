---
title:                "Tạo một tập tin tạm thời"
aliases: - /vi/php/creating-a-temporary-file.md
date:                  2024-01-28T21:58:49.112476-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tạo một tập tin tạm thời"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/creating-a-temporary-file.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái Gì & Tại Sao?
Việc tạo một tệp tạm thời trong PHP có nghĩa là tạo ra một tệp sẽ tồn tại trong thời gian ngắn để bạn sử dụng, sau đó, cái tệp đó sẽ biến mất. Tại sao lại làm vậy? Điều đó tuyệt vời cho việc xử lý các khối dữ liệu trong quá trình xử lý, giữ thông tin nhạy cảm không bị lưu trữ trên đĩa, và đảm bảo không có dấu vết nào còn lại sau khi script của bạn kết thúc.

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
