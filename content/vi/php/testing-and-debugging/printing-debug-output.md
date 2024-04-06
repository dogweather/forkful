---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:00.301008-07:00
description: "L\xE0m th\u1EBF n\xE0o: Th\xF4ng tin g\u1EE1 l\u1ED7i kh\xF4ng c\xF3\
  \ nhi\u1EC1u thay \u0111\u1ED5i: n\xF3 \u0111\xE3 t\u1ED3n t\u1EA1i t\u1EEB nh\u1EEF\
  ng ng\xE0y \u0111\u1EA7u khi c\xE1c l\u1EADp tr\xECnh vi\xEAn c\u1ED5 \u0111i\u1EC3\
  n debug b\u1EB1ng l\u1EC7nh printf(). PHP \u0111\xE3\u2026"
lastmod: '2024-04-05T22:50:51.096249-06:00'
model: gpt-4-0125-preview
summary: "Th\xF4ng tin g\u1EE1 l\u1ED7i kh\xF4ng c\xF3 nhi\u1EC1u thay \u0111\u1ED5\
  i."
title: "In ra th\xF4ng tin g\u1EE1 l\u1ED7i"
weight: 33
---

## Làm thế nào:
```PHP
<?php
// Xuất kết quả cơ bản
$variable = 'Gỡ lỗi thật tuyệt!';
echo $variable;

// Sử dụng print_r cho các mảng
$myArray = ['apple', 'orange', 'banana'];
echo '<pre>'; // Làm cho nó dễ đọc
print_r($myArray);
echo '</pre>';

// var_dump để xem chi tiết
$anotherArray = ['key' => 'value', 'anotherKey' => 123];
var_dump($anotherArray);

// Gửi vào nhật ký lỗi
error_log('Nội dung này được gửi vào nhật ký để gỡ lỗi một cách kín đáo.');
?>
```
Đầu ra Mẫu:
```
Gỡ lỗi thật tuyệt!
Mảng
(
    [0] => apple
    [1] => orange
    [2] => banana
)
mảng(2) {
  ["key"]=>
  chuỗi(5) "value"
  ["anotherKey"]=>
  int(123)
}
```

## Đào Sâu:
Thông tin gỡ lỗi không có nhiều thay đổi: nó đã tồn tại từ những ngày đầu khi các lập trình viên cổ điển debug bằng lệnh printf(). PHP đã tiếp nhận điều này với `echo`, `print`, `print_r()`, và `var_dump()`. Có thể không hoành tráng, nhưng nó hoạt động. Các lập trình viên PHP hiện đại cũng có Xdebug, có thể bước qua mã lệnh và hiển thị thông tin một cách đẹp đẽ hơn. Đối với nhật ký, bạn có `error_log()`, một cách đưa thông điệp vào nhật ký máy chủ mà không tiết lộ chúng với người dùng. Mỗi công cụ đều có chỗ đứng của nó: `echo` và `print` nhanh và sơ sài; `print_r()` cho cái nhìn thân thiện với con người về mảng; `var_dump()` cung cấp chi tiết nền tảng về kiểu và độ dài; `error_log()` giữ mọi thứ kín đáo khi bạn đang trong chế độ thám tử trên một trang web đang hoạt động.

## Xem Thêm:
- Tài liệu PHP về `echo`: https://www.php.net/manual/en/function.echo.php
- Thêm thông tin về `print_r()`: https://www.php.net/manual/en/function.print-r.php
- Chi tiết khó khăn của `var_dump()`: https://www.php.net/manual/en/function.var-dump.php
- Lặn vào việc ghi nhật ký với `error_log()`: https://www.php.net/manual/en/function.error-log.php
- Xdebug, người bạn tốt nhất của người gỡ lỗi: https://xdebug.org/docs/display
