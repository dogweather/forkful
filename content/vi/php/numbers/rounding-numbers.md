---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:03.912046-07:00
description: "L\xE0m sao: PHP cung c\u1EA5p m\u1ED9t v\xE0i c\xE1ch \u0111\u1EC3 l\xE0\
  m tr\xF2n s\u1ED1: `round()`, `ceil()`, v\xE0 `floor()`. D\u01B0\u1EDBi \u0111\xE2\
  y l\xE0 c\xE1ch ch\xFAng ho\u1EA1t \u0111\u1ED9ng."
lastmod: '2024-03-13T22:44:36.759477-06:00'
model: gpt-4-0125-preview
summary: "PHP cung c\u1EA5p m\u1ED9t v\xE0i c\xE1ch \u0111\u1EC3 l\xE0m tr\xF2n s\u1ED1\
  ."
title: "L\xE0m tr\xF2n s\u1ED1"
weight: 13
---

## Làm sao:
PHP cung cấp một vài cách để làm tròn số: `round()`, `ceil()`, và `floor()`. Dưới đây là cách chúng hoạt động:

```php
echo round(3.14159);   // Trả về 3
echo round(3.14159, 2); // Trả về 3.14

echo ceil(3.14159);    // Trả về 4, luôn làm tròn lên

echo floor(3.14159);   // Trả về 3, luôn làm tròn xuống
```

## Sâu hơn
Việc làm tròn số đã trở thành một phần cơ bản trong toán học và tính toán từ thời cổ đại để xử lý với các số thập phân vô hạn không thực tế. Trong PHP, `round()` có thể nhận một tham số độ chính xác và chế độ, ảnh hưởng đến hành vi của nó – `PHP_ROUND_HALF_UP`, `PHP_ROUND_HALF_DOWN`, v.v., xác định cách nó sẽ hoạt động khi gặp tình huống ".5". Độ chính xác là quan trọng trong các ứng dụng tài chính nơi việc làm tròn có thể được điều chỉnh theo luật, ảnh hưởng đến cách `round()` được thực thi trong code.

Các phương pháp làm tròn tự tạo hoặc các hàm BC Math cho các phép toán số học với độ chính xác tùy ý là các lựa chọn thay thế cho các hàm được tích hợp sẵn, chúng hữu ích cho các tình huống cần kiểm soát nhiều hơn hoặc đối phó với các số rất lớn mà độ chính xác bản địa có thể không ổn định.

## Xem thêm
Khám phá thêm trong tài liệu PHP:
- [Hàm `round` trong PHP](https://php.net/manual/en/function.round.php)
- [Hàm `ceil` trong PHP](https://php.net/manual/en/function.ceil.php)
- [Hàm `floor` trong PHP](https://php.net/manual/en/function.floor.php)
- [BC Math cho số học độ chính xác tùy ý](https://php.net/manual/en/book.bc.php)
