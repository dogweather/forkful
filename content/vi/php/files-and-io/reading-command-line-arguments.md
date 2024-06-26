---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:54.561842-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: PHP s\u1EED d\u1EE5ng m\u1ED9t m\u1EA3\
  ng to\xE0n c\u1EE5c `$argv` \u0111\u1EC3 l\u01B0u tr\u1EEF c\xE1c \u0111\u1ED1i\
  \ s\u1ED1 d\xF2ng l\u1EC7nh, v\u1EDBi `$argv[0]` l\xE0 t\xEAn script. \u0110\xE2\
  y l\xE0 c\xE1ch b\u1EA1n s\u1EED d\u1EE5ng n\xF3."
lastmod: '2024-03-13T22:44:36.787116-06:00'
model: gpt-4-0125-preview
summary: "PHP s\u1EED d\u1EE5ng m\u1ED9t m\u1EA3ng to\xE0n c\u1EE5c `$argv` \u0111\
  \u1EC3 l\u01B0u tr\u1EEF c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh, v\u1EDB\
  i `$argv[0]` l\xE0 t\xEAn script."
title: "\u0110\u1ECDc c\xE1c \u0111\u1ED1i s\u1ED1 d\xF2ng l\u1EC7nh"
weight: 23
---

## Cách thực hiện:
PHP sử dụng một mảng toàn cục `$argv` để lưu trữ các đối số dòng lệnh, với `$argv[0]` là tên script. Đây là cách bạn sử dụng nó:

```php
<?php
// kiểm tra xem có đối số nào được truyền không
if ($argc > 1) {
    echo "Xin chào, " . $argv[1] . "!\n";
} else {
    echo "Xin chào, người lạ mặt!\n";
}
?>
```

Nếu bạn gọi script này là `sayhello.php` và chạy `php sayhello.php World`, kết quả sẽ là:

```
Xin chào, World!
```

Không có đối số nào? Bạn sẽ nhận được:

```
Xin chào, người lạ mặt!
```

## Tìm hiểu sâu hơn
Theo lịch sử, các script dòng lệnh đã là nền tảng của tự động hóa hệ thống, trước khi các GUI chiếm lĩnh. PHP, mặc dù được sử dụng rộng rãi cho phát triển web, cũng cung cấp hỗ trợ CLI mạnh mẽ.

Hai cách chính để đọc đối số trong PHP là `$argv` và hàm `getopt()`. `$argv` là một mảng đơn giản trong khi `getopt()` cung cấp chức năng phức tạp hơn, như phân tích các tùy chọn (có hoặc không có giá trị).

Về việc triển khai, `$argv` và `$argc` (số lượng đối số) tự động có sẵn khi ở chế độ CLI — không cần thiết lập thêm. Chúng không có mặt khi chạy script web PHP vì đó không phải là sân chơi của chúng.

Nhưng nhớ rằng, nếu bạn đăng ký `argv` và `argc` như là các biến toàn cục thông qua `php.ini` hoặc cấu hình máy chủ, chúng cũng có thể được truy cập trong script web. Tuy nhiên, điều này hiếm khi cần thiết và có thể là một rủi ro bảo mật.

## Xem thêm
Để phân tích lệnh dòng lệnh phức tạp hơn:
- [PHP.net getopt](https://www.php.net/manual/en/function.getopt.php)

Để tìm hiểu về máy chủ CLI của PHP:
- [PHP.net Sử dụng dòng lệnh](https://www.php.net/manual/en/features.commandline.php)

Tương tác với cộng đồng PHP:
- [Thảo luận về PHP CLI trên Stack Overflow](https://stackoverflow.com/questions/tagged/php+cli)
