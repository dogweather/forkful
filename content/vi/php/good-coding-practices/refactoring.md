---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:21.326151-07:00
description: "Refactoring l\xE0 qu\xE1 tr\xECnh c\u1EA5u tr\xFAc l\u1EA1i m\xE3 m\xE1\
  y t\xEDnh hi\u1EC7n c\xF3 m\xE0 kh\xF4ng thay \u0111\u1ED5i h\xE0nh vi b\xEAn ngo\xE0\
  i c\u1EE7a n\xF3. L\u1EADp tr\xECnh vi\xEAn refactor \u0111\u1EC3 c\u1EA3i thi\u1EC7\
  n c\xE1c thu\u1ED9c t\xEDnh\u2026"
lastmod: '2024-03-13T22:44:36.778203-06:00'
model: gpt-4-0125-preview
summary: "Refactoring l\xE0 qu\xE1 tr\xECnh c\u1EA5u tr\xFAc l\u1EA1i m\xE3 m\xE1\
  y t\xEDnh hi\u1EC7n c\xF3 m\xE0 kh\xF4ng thay \u0111\u1ED5i h\xE0nh vi b\xEAn ngo\xE0\
  i c\u1EE7a n\xF3."
title: "T\xE1i c\u1EA5u tr\xFAc m\xE3"
weight: 19
---

## Làm thế nào:
Hãy lấy một đoạn PHP điển hình và áp dụng một số phép màu refactoring vào nó.

Trước khi refactoring, mã của chúng ta có thể trông như thế này:

```php
function printOrderDetails($order) {
    foreach ($order as $item) {
        echo "Item: " . $item['name'];
        echo " - Price: " . $item['price'];
        echo "<br>";
    }
    
    if (!empty($order)) {
        echo "Total: " . array_sum(array_column($order, 'price'));
    }
}
```

Nhưng chúng ta có thể refactor đoạn mã này để cải thiện sự rõ ràng và tính module của nó:

```php
function printItem($item) {
    echo "Item: {$item['name']} - Price: {$item['price']}<br>";
}

function calculateTotal($order) {
    return array_sum(array_column($order, 'price'));
}

function printOrderDetails(array $order) {
    array_walk($order, 'printItem');

    if (!empty($order)) {
        echo "Total: " . calculateTotal($order);
    }
}
```
Bằng cách phân chia hàm `printOrderDetails` thành các hàm nhỏ hơn, mã của chúng ta trở nên dễ đọc và dễ gỡ lỗi hơn.

## Sâu hơn
Refactoring có nguồn gốc từ cộng đồng lập trình smalltalk vào đầu những năm 1990 và được phổ biến thêm bởi cuốn sách điển hình của Martin Fowler "Refactoring: Improving the Design of Existing Code" (1999). Dù refactoring có thể được áp dụng cho bất kỳ ngôn ngữ lập trình nào, bản chất động của PHP cho phép một số thách thức và cơ hội độc đáo.

Các phương án thay thế cho việc refactoring có thể bao gồm việc viết lại mã từ đầu, điều này thường rủi ro và tốn thời gian hơn. Trong hệ sinh thái PHP, các công cụ như PHPStan và Rector có thể tự động nhận diện và thực hiện một số hoạt động refactoring, tương ứng. Về mặt thực hiện, việc giữ cho refactoring nhỏ gọn và thử nghiệm rộng rãi với các unit test là những thực hành chính để đảm bảo refactoring thành công mà không giới thiệu lỗi.

## Xem thêm
- Sách Refactoring của Martin Fowler: https://martinfowler.com/books/refactoring.html
- PHPStan, một công cụ phân tích tĩnh PHP: https://phpstan.org/
- Rector, một công cụ cho refactoring tự động của mã PHP: https://getrector.org/
- Kiểm thử đơn vị PHP với PHPUnit: https://phpunit.de/
