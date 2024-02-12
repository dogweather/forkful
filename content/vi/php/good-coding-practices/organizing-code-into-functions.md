---
title:                "Sắp xếp mã thành các hàm"
aliases: - /vi/php/organizing-code-into-functions.md
date:                  2024-01-28T22:03:26.813194-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sắp xếp mã thành các hàm"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/organizing-code-into-functions.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Tổ chức code thành các hàm là việc phân chia mã lệnh của bạn thành các khối tái sử dụng với mục đích xác định. Chúng ta làm điều này để giữ mọi thứ gọn gàng, tránh lặp lại và làm cho việc gỡ lỗi trở nên dễ dàng.

## Cách thực hiện:
Hãy tưởng tượng chúng ta có đoạn mã lặp lại cho việc chào hỏi người dùng. Thay vào đó, chúng tôi sẽ gói nó trong một hàm như `greet_user`:

```php
function greet_user($name) {
    return "Hello, " . $name . "!";
}

echo greet_user("Alice");
echo greet_user("Bob");
```

Kết quả:
```
Hello, Alice!
Hello, Bob!
```

Bây giờ, bạn đã có một công cụ tiện lợi mà bạn có thể sử dụng bất cứ khi nào mà không cần phải viết lại cùng một dòng mã mỗi khi bạn muốn nói xin chào.

## Soi sâu hơn
Hàm đã có trong lập trình kể từ những ngày đầu của FORTRAN vào những năm '50. Chúng là một trụ cột của lập trình có cấu trúc và tất cả về tính module và cô lập. Có cách khác? Tốt, bạn có thể đi theo hướng hướng đối tượng và nói về lớp và phương thức, đó là các hàm với bộ suit sang trọng. Đối với PHP, chi tiết triển khai bao gồm việc chỉ định giá trị mặc định cho các tham số, gợi ý kiểu cho đầu vào và có khả năng trả về nhiều giá trị bằng cách sử dụng một mảng hoặc, từ PHP 7.1 trở đi, một danh sách.

Dưới đây là một bước ngoặt hiện đại với việc khai báo kiểu và giá trị mặc định:

```php
function add(float $a, float $b = 0.0): float {
    return $a + $b;
}

echo add(1.5);
echo add(1.5, 2.5);
```

PHP 7.4 đã đưa vào hàm mũi tên, giúp viết các hàm một dòng một cách ngắn gọn, thường được sử dụng trong các hoạt động mảng:

```php
$numbers = array(1, 2, 3, 4);
$squared = array_map(fn($n) => $n * $n, $numbers);
print_r($squared);
```

Kết quả:
```
Array
(
    [0] => 1
    [1] => 4
    [2] => 9
    [3] => 16
)
```

## Xem thêm
- [Hướng dẫn PHP về Hàm](https://www.php.net/manual/en/functions.user-defined.php)
- [PHP: Cách Đúng - Hàm](https://phptherightway.com/#functions)
- [Học về Hàm Mũi Tên PHP 7.4](https://stitcher.io/blog/short-closures-in-php)
