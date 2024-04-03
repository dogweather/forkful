---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:38.813328-07:00
description: "L\xE0m th\u1EBF n\xE0o: S\u1EED d\u1EE5ng h\xE0m `strlen()` nh\u01B0\
  \ sau."
lastmod: '2024-03-13T22:44:36.754171-06:00'
model: gpt-4-0125-preview
summary: "S\u1EED d\u1EE5ng h\xE0m `strlen()` nh\u01B0 sau."
title: "T\xECm chi\u1EC1u d\xE0i c\u1EE7a m\u1ED9t chu\u1ED7i k\xFD t\u1EF1"
weight: 7
---

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
