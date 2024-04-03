---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:47.388337-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: Trong PHP, b\u1EA1n c\xF3 th\u1EC3 n\u1ED9\
  i suy chu\u1ED7i s\u1EED d\u1EE5ng d\u1EA5u ngo\u1EB7c k\xE9p ho\u1EB7c c\xFA ph\xE1\
  p heredoc."
lastmod: '2024-03-13T22:44:36.747550-06:00'
model: gpt-4-0125-preview
summary: "Trong PHP, b\u1EA1n c\xF3 th\u1EC3 n\u1ED9i suy chu\u1ED7i s\u1EED d\u1EE5\
  ng d\u1EA5u ngo\u1EB7c k\xE9p ho\u1EB7c c\xFA ph\xE1p heredoc."
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
weight: 8
---

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
