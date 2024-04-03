---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:26.813194-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: H\xE3y t\u01B0\u1EDFng t\u01B0\u1EE3ng\
  \ ch\xFAng ta c\xF3 \u0111o\u1EA1n m\xE3 l\u1EB7p l\u1EA1i cho vi\u1EC7c ch\xE0\
  o h\u1ECFi ng\u01B0\u1EDDi d\xF9ng. Thay v\xE0o \u0111\xF3, ch\xFAng t\xF4i s\u1EBD\
  \ g\xF3i n\xF3 trong m\u1ED9t h\xE0m nh\u01B0 `greet_user`."
lastmod: '2024-03-13T22:44:36.774359-06:00'
model: gpt-4-0125-preview
summary: "H\xE3y t\u01B0\u1EDFng t\u01B0\u1EE3ng ch\xFAng ta c\xF3 \u0111o\u1EA1n\
  \ m\xE3 l\u1EB7p l\u1EA1i cho vi\u1EC7c ch\xE0o h\u1ECFi ng\u01B0\u1EDDi d\xF9ng."
title: "S\u1EAFp x\u1EBFp m\xE3 th\xE0nh c\xE1c h\xE0m"
weight: 18
---

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
