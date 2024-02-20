---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:47.388337-07:00
description: "N\u1ED9i suy chu\u1ED7i cho ph\xE9p b\u1EA1n ch\xE8n tr\u1EF1c ti\u1EBF\
  p gi\xE1 tr\u1ECB c\u1EE7a bi\u1EBFn v\xE0o trong m\u1ED9t chu\u1ED7i. L\u1EADp\
  \ tr\xECnh vi\xEAn s\u1EED d\u1EE5ng n\xF3 \u0111\u1EC3 d\u1EC7t c\xE1c bi\u1EBF\
  n v\xE0o v\u0103n b\u1EA3n, l\xE0m cho m\xE3 ngu\u1ED3n\u2026"
lastmod: 2024-02-19 22:04:55.929192
model: gpt-4-0125-preview
summary: "N\u1ED9i suy chu\u1ED7i cho ph\xE9p b\u1EA1n ch\xE8n tr\u1EF1c ti\u1EBF\
  p gi\xE1 tr\u1ECB c\u1EE7a bi\u1EBFn v\xE0o trong m\u1ED9t chu\u1ED7i. L\u1EADp\
  \ tr\xECnh vi\xEAn s\u1EED d\u1EE5ng n\xF3 \u0111\u1EC3 d\u1EC7t c\xE1c bi\u1EBF\
  n v\xE0o v\u0103n b\u1EA3n, l\xE0m cho m\xE3 ngu\u1ED3n\u2026"
title: "N\u1ED9i suy chu\u1ED7i k\xFD t\u1EF1"
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
