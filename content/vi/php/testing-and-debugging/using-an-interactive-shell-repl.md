---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:15.227701-07:00
description: "L\xE0m th\u1EBF n\xE0o: Kh\u1EDFi ch\u1EA1y REPL PHP b\u1EB1ng c\xE1\
  ch ch\u1EA1y `php -a` trong terminal c\u1EE7a b\u1EA1n. D\u01B0\u1EDBi \u0111\xE2\
  y l\xE0 m\u1ED9t v\xED d\u1EE5 v\u1EC1 c\xE1ch n\xF3 ho\u1EA1t \u0111\u1ED9ng."
lastmod: '2024-03-13T22:44:36.768689-06:00'
model: gpt-4-0125-preview
summary: "Kh\u1EDFi ch\u1EA1y REPL PHP b\u1EB1ng c\xE1ch ch\u1EA1y `php -a` trong\
  \ terminal c\u1EE7a b\u1EA1n."
title: "S\u1EED d\u1EE5ng v\u1ECF t\u01B0\u01A1ng t\xE1c (REPL)"
weight: 34
---

## Làm thế nào:
Khởi chạy REPL PHP bằng cách chạy `php -a` trong terminal của bạn. Dưới đây là một ví dụ về cách nó hoạt động:

```php
php > echo "Hello, World!";
Hello, World!
php > $arr = [1, 2, 3];
php > print_r($arr);
Mảng
(
    [0] => 1
    [1] => 2
    [2] => 3
)
```

Bạn cũng có thể định nghĩa hàm:

```php
php > function sum($a, $b) { return $a + $b; }
php > echo sum(5, 10);
15
```

## Sâu hơn nữa
REPL đã xuất hiện dưới một số hình thức kể từ những ngày đầu của LISP vào những năm 1960. Shell tương tác của PHP kém phát triển so với những ngôn ngữ như Python hay JavaScript. Nó không duy trì trạng thái giữa các phiên và thiếu các tính năng như tự động hoàn thành. Để có một REPL PHP đầy đủ tính năng hơn, hãy xem xét các lựa chọn thay thế như `psysh` hay `boris`. Những shell của bên thứ ba này cung cấp công cụ kiểm tra tốt hơn, hoàn thành tab và thậm chí là một trình gỡ lỗi.

Bên dưới cùng, REPL của PHP hoạt động bằng cách biên dịch và thực thi từng dòng mã khi nó được nhập. Hạn chế của phương pháp này trở nên rõ ràng với những vấn đề như khai báo lại lớp, điều này không thể thực hiện trong cùng một phiên. Nó tốt cho các bài kiểm tra đơn giản nhưng có thể trở nên rườm rà với các nhiệm vụ phức tạp.

## Xem thêm
- [Hướng dẫn PHP - Shell tương tác](https://www.php.net/manual/en/features.commandline.interactive.php)
- [PsySH: Một bảng điều khiển phát triển runtime, gỡ lỗi tương tác và REPL cho PHP](https://psysh.org/)
- [Boris: Một REPL nhỏ cho PHP](https://github.com/borisrepl/boris)
