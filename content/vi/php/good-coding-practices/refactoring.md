---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:21.326151-07:00
description: "L\xE0m th\u1EBF n\xE0o: H\xE3y l\u1EA5y m\u1ED9t \u0111o\u1EA1n PHP\
  \ \u0111i\u1EC3n h\xECnh v\xE0 \xE1p d\u1EE5ng m\u1ED9t s\u1ED1 ph\xE9p m\xE0u refactoring\
  \ v\xE0o n\xF3. Tr\u01B0\u1EDBc khi refactoring, m\xE3 c\u1EE7a ch\xFAng ta c\xF3\
  \ th\u1EC3 tr\xF4ng nh\u01B0 th\u1EBF n\xE0y."
lastmod: '2024-03-13T22:44:36.778203-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y l\u1EA5y m\u1ED9t \u0111o\u1EA1n PHP \u0111i\u1EC3n h\xECnh v\xE0\
  \ \xE1p d\u1EE5ng m\u1ED9t s\u1ED1 ph\xE9p m\xE0u refactoring v\xE0o n\xF3."
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
