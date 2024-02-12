---
title:                "Viết các bài kiểm tra"
aliases:
- vi/php/writing-tests.md
date:                  2024-01-28T22:13:18.220904-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết các bài kiểm tra"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/writing-tests.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Kiểm thử đảm bảo mã của bạn hoạt động như mong đợi. Nó giúp tiết kiệm thời gian bằng cách phát hiện lỗi sớm và đảm bảo các thay đổi mã không làm hỏng mọi thứ.

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
