---
title:                "Tái cấu trúc mã"
date:                  2024-01-28T22:06:21.326151-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tái cấu trúc mã"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/refactoring.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Refactoring là quá trình cấu trúc lại mã máy tính hiện có mà không thay đổi hành vi bên ngoài của nó. Lập trình viên refactor để cải thiện các thuộc tính phi chức năng của phần mềm, làm cho mã sạch hơn, hiệu quả hơn và dễ dàng bảo trì hơn.

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