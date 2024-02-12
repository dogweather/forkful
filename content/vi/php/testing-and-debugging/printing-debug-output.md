---
title:                "In ra thông tin gỡ lỗi"
aliases: - /vi/php/printing-debug-output.md
date:                  2024-01-28T22:05:00.301008-07:00
model:                 gpt-4-0125-preview
simple_title:         "In ra thông tin gỡ lỗi"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/printing-debug-output.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
In ra thông tin gỡ lỗi là việc đưa dữ liệu nội bộ của mã lệnh lên màn hình để hiểu xem đang xảy ra điều gì. Các lập trình viên làm việc này để kiểm tra tính hợp lý, tìm nơi côn trùng ẩn náu hoặc chỉ đơn giản là để xem mã lệnh có đang hoạt động như dự định hay không.

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
