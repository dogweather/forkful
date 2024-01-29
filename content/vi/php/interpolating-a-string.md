---
title:                "Nội suy chuỗi ký tự"
date:                  2024-01-28T22:02:47.388337-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nội suy chuỗi ký tự"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Nội suy chuỗi cho phép bạn chèn trực tiếp giá trị của biến vào trong một chuỗi. Lập trình viên sử dụng nó để dệt các biến vào văn bản, làm cho mã nguồn trở nên sạch sẽ và dễ đọc hơn.

## Cách thực hiện:

Trong PHP, bạn có thể nội suy chuỗi sử dụng dấu ngoặc kép hoặc cú pháp heredoc:

```php
$name = "World";
echo "Hello, $name!"; // Kết quả: Hello, World!

// Sử dụng ngoặc nhọn cho các biến phức tạp hơn
$object = new stdClass();
$object->greeting = "Hello";
echo "{$object->greeting}, $name!"; // Kết quả: Hello, World!

// Cú pháp Heredoc cho chuỗi nhiều dòng
$heredoc = <<<EOT
Đây là một chuỗi chứa $name bên trong nó.
Bạn có thể viết bao nhiêu tùy thích ở đây.
EOT;
echo $heredoc; // Kết quả: Đây là một chuỗi chứa World bên trong nó.
```

Lưu ý: Dấu ngoặc đơn không nội suy:

```php
echo 'Hello, $name!'; // Kết quả: Hello, $name!
```

## Sâu hơn:

Trước khi PHP giới thiệu nội suy, sử dụng toán tử dấu chấm (.) để nối chuỗi là cách thức duy nhất. Ví dụ:

```php
echo 'Hello, ' . $name . '!';
```

Nội suy làm đơn giản hóa quy trình này bằng cách phân tích trực tiếp biến trong chuỗi.

Nội suy chuỗi đã xuất hiện từ PHP 4, nhưng việc sử dụng các biểu thức phức tạp trong ngoặc nhọn trở nên linh hoạt hơn với PHP 7. Với những cải tiến này, PHP đã làm cho việc nhúng bất kỳ biến nào, bao gồm cả thuộc tính đối tượng và phần tử mảng, vào trong một chuỗi trở nên dễ dàng hơn.

Các phương pháp thay thế cho nội suy tồn tại, chẳng hạn như sử dụng `sprintf()` cho chuỗi định dạng hoặc `implode()` cho mảng. Đôi khi chúng có thể cung cấp nhiều kiểm soát hơn đối với định dạng chuỗi, đặc biệt là cho việc địa phương hóa và cấu trúc phức tạp.

Về mặt triển khai, PHP tìm kiếm các biến bên trong chuỗi khi chúng ở trong dấu ngoặc kép hoặc cú pháp heredoc và thay thế chúng bằng giá trị của biến. Bộ phân tích cú pháp bỏ qua dấu đô la ($) trong chuỗi được bao bởi dấu ngoặc đơn, coi nó như một ký tự bình thường.

## Xem thêm

- [PHP: Chuỗi](http://php.net/manual/en/language.types.string.php) - Tài liệu chính thức của PHP về chuỗi.
- [PHP: Cú pháp Heredoc](https://www.php.net/manual/en/language.types.string.php#language.types.string.syntax.heredoc) - Phần chi tiết về Heredoc trong tài liệu của PHP.
- [PHP: Toán tử Chuỗi](https://www.php.net/manual/en/language.operators.string.php) - Thêm thông tin về nối chuỗi và toán tử dấu chấm.
- [PHP: sprintf](https://www.php.net/manual/en/function.sprintf.php) - Tài liệu của hàm `sprintf()` cho định dạng chuỗi.
