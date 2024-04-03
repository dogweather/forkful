---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:20.426819-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong PHP, `is_dir()` ki\u1EC3m tra n\u1EBF\
  u m\u1ED9t th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i."
lastmod: '2024-03-13T22:44:36.785847-06:00'
model: gpt-4-0125-preview
summary: "Trong PHP, `is_dir()` ki\u1EC3m tra n\u1EBFu m\u1ED9t th\u01B0 m\u1EE5c\
  \ c\xF3 t\u1ED3n t\u1EA1i."
title: "Ki\u1EC3m tra xem th\u01B0 m\u1EE5c c\xF3 t\u1ED3n t\u1EA1i kh\xF4ng"
weight: 20
---

## Cách thực hiện:
Trong PHP, `is_dir()` kiểm tra nếu một thư mục có tồn tại:

```PHP
$directory = "/path/to/dir";

if (is_dir($directory)) {
    echo "Thư mục tồn tại.";
} else {
    echo "Thư mục không tồn tại.";
}
```

Kết quả mẫu:
```
Thư mục tồn tại.
```
Hoặc, nếu thư mục thực sự không tồn tại:
```
Thư mục không tồn tại.
```

Để giảm thiểu lỗi và sử dụng kiểm tra chi tiết hơn, kết hợp `is_dir()` với hàm `file_exists()`:

```PHP
$directory = "/path/to/dir";

if (file_exists($directory) && is_dir($directory)) {
    echo "Thư mục tồn tại và là một thư mục.";
} else {
    echo "Thư mục không tồn tại hoặc là một tập tin.";
}
```

## Sâu hơn
`is_dir()` đã có trong PHP từ phiên bản 4.0.0, cho phép kiểm tra sự tồn tại của thư mục trước các thao tác có thể thất bại hoặc gây ra lỗi. Không được nhầm lẫn với `file_exists()`, kiểm tra cả tập tin và thư mục cùng một lúc, `is_dir()` dành riêng cho thư mục.

Trước khi có các hàm tích hợp này, lập trình viên có thể đã sử dụng `opendir()` và kiểm tra giá trị trả về là false để suy luận về sự không tồn tại. Điều này ít hiệu quả và dễ gặp lỗi hơn.

Phía sau hậu trường, `is_dir()` thực hiện một syscall đến hệ thống tập tin cơ bản, điều này có thể tốn kém hơn về mặt hoạt động I/O, đặc biệt là đối với hệ thống tập tin từ xa hoặc ảo. Lưu trữ kết quả hoặc cấu trúc mã để giảm thiểu việc kiểm tra sự tồn tại có thể tối ưu hóa hiệu suất.

Một phương án khác, đặc biệt phù hợp trong các hệ thống giống Unix, là sử dụng `exec()` với một lệnh hệ thống như `ls` hoặc `test -d`, nhưng điều này đưa vào gánh nặng của việc kích hoạt một shell và ít di động hơn.

## Xem Thêm
- [PHP Manual: `is_dir()`](https://www.php.net/manual/en/function.is-dir.php)
- [PHP Manual: `file_exists()`](https://www.php.net/manual/en/function.file-exists.php)
- [Các phương pháp tốt nhất với hệ thống tập tin trong PHP](https://www.php-fig.org/psr/psr-4/)
- [Các hàm hệ thống tập tin trong PHP](https://www.php.net/manual/en/ref.filesystem.php)
