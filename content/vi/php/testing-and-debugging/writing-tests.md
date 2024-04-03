---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:18.220904-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Ch\xFAng ta s\u1EBD t\xECm hi\u1EC3u PHPUnit,\
  \ m\u1ED9t framework ki\u1EC3m th\u1EED PHP ph\u1ED5 bi\u1EBFn. \u0110\u1EA7u ti\xEA\
  n, c\xE0i \u0111\u1EB7t n\xF3 b\u1EB1ng Composer."
lastmod: '2024-03-13T22:44:36.771270-06:00'
model: gpt-4-0125-preview
summary: "Ch\xFAng ta s\u1EBD t\xECm hi\u1EC3u PHPUnit, m\u1ED9t framework ki\u1EC3\
  m th\u1EED PHP ph\u1ED5 bi\u1EBFn."
title: "Vi\u1EBFt c\xE1c b\xE0i ki\u1EC3m tra"
weight: 36
---

## Cách thực hiện:
Chúng ta sẽ tìm hiểu PHPUnit, một framework kiểm thử PHP phổ biến. Đầu tiên, cài đặt nó bằng Composer:

```bash
composer require --dev phpunit/phpunit
```

Bây giờ, hãy viết một bài kiểm thử đơn giản. Hãy tưởng tượng bạn có một lớp `Calculator` với phương thức `add`.

```php
// Calculator.php
class Calculator {
    public function add($a, $b) {
        return $a + $b;
    }
}
```

Đây là cách bạn kiểm thử nó:

```php
// CalculatorTest.php
use PHPUnit\Framework\TestCase;

class CalculatorTest extends TestCase {
    public function testAddition() {
        $calculator = new Calculator();
        $this->assertEquals(4, $calculator->add(2, 2));
    }
}
```

Chạy bài kiểm thử với:

```bash
./vendor/bin/phpunit CalculatorTest
```

Kết quả hiện ra sẽ cho thấy bài kiểm thử đạt hay thất bại.

## Sâu hơn
Kiểm thử không luôn là vấn đề lớn trong PHP. Ban đầu, nhiều người chỉ ghép mã lại và kiểm tra thủ công xem nó có hoạt động không. Bây giờ, kiểm thử chính là vua. PHPUnit bắt đầu trở nên phổ biến vào những năm 2000 và bây giờ gần như là tiêu chuẩn. Có phương án khác không? Chắc chắn là có, ví dụ như PHPSpec và Behat, để bắt đầu. Bên trong, PHPUnit sử dụng các phát biểu để so sánh kết quả mong đợi và thực tế, và giả lập đôi (mocks, stubs, spies) để mô phỏng các phụ thuộc bên ngoài.

## Xem thêm
- Hướng dẫn PHPUnit: https://phpunit.de/manual/current/en/index.html
- PHP Cách Đúng (Kiểm thử): http://www.phptherightway.com/#testing
- Mockery (framework giả lập cho PHPUnit): http://docs.mockery.io/en/latest/
