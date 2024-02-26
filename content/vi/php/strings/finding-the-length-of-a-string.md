---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:38.813328-07:00
description: "T\xECm \u0111\u1ED9 d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i ngh\u0129a l\xE0\
  \ x\xE1c \u0111\u1ECBnh xem n\xF3 bao g\u1ED3m bao nhi\xEAu k\xFD t\u1EF1. C\xE1\
  c l\u1EADp tr\xECnh vi\xEAn th\u01B0\u1EDDng c\u1EA7n th\xF4ng tin n\xE0y cho c\xE1\
  c nhi\u1EC7m v\u1EE5 nh\u01B0 x\xE1c th\u1EF1c nh\u1EADp\u2026"
lastmod: '2024-02-25T18:49:35.099201-07:00'
model: gpt-4-0125-preview
summary: "T\xECm \u0111\u1ED9 d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i ngh\u0129a l\xE0\
  \ x\xE1c \u0111\u1ECBnh xem n\xF3 bao g\u1ED3m bao nhi\xEAu k\xFD t\u1EF1. C\xE1\
  c l\u1EADp tr\xECnh vi\xEAn th\u01B0\u1EDDng c\u1EA7n th\xF4ng tin n\xE0y cho c\xE1\
  c nhi\u1EC7m v\u1EE5 nh\u01B0 x\xE1c th\u1EF1c nh\u1EADp\u2026"
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Tìm độ dài của một chuỗi nghĩa là xác định xem nó bao gồm bao nhiêu ký tự. Các lập trình viên thường cần thông tin này cho các nhiệm vụ như xác thực nhập liệu, quản lý chuỗi con, hoặc đơn giản chỉ định dạng đầu ra.

## Làm thế nào:

Sử dụng hàm `strlen()` như sau:

```php
<?php
$text = "Hello, world!";
$length = strlen($text);
echo $length; // Đầu ra: 13
?>
```

Nếu bạn chạy đoạn mã này, bạn sẽ thấy `13` trên màn hình của mình vì "Hello, world!" dài 13 ký tự, bao gồm cả khoảng trống và dấu chấm than.

## Sâu hơn

Hàm `strlen()` đã là một phần của PHP từ những phiên bản đầu tiên. Nó đơn giản và hoạt động dựa trên số byte, thường tương đương với số lượng ký tự trong chuỗi không cần xem xét mã hóa đặc biệt.

Tuy nhiên, với sự quốc tế hóa của các ứng dụng web, việc đối phó với nhiều ngôn ngữ và mã hóa ký tự trở nên phổ biến. Ví dụ, các ký tự trong UTF-8 có thể sử dụng nhiều hơn một byte. Đó là nơi `mb_strlen()` xuất hiện:

```php
<?php
// Một chuỗi với các ký tự đa byte
$multibyteText = "こんにちは";
$length = mb_strlen($multibyteText, "UTF-8");
echo $length; // Đầu ra: 5
?>
```

Năm ký tự, nhưng nhiều byte hơn. Hàm `mb_strlen()` tôn trọng mã hóa ký tự, đảm bảo kiểm tra độ dài chính xác cho các chuỗi đa byte.

`strlen()` nhanh và phù hợp với các bộ ký tự một byte. `mb_strlen()`, mặc dù hơi chậm hơn do cần xử lý mã hóa phức tạp hơn, là cần thiết khi làm việc với văn bản quốc tế hóa.

## Xem thêm

- [Tài liệu chính thức PHP `strlen()`](https://www.php.net/manual/en/function.strlen.php)
- [Tài liệu chính thức PHP `mb_strlen()`](https://www.php.net/manual/en/function.mb-strlen.php)
- [Phần mở rộng Chuỗi Đa Byte PHP](https://www.php.net/manual/en/book.mbstring.php)
