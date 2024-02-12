---
title:                "Tìm chiều dài của một chuỗi ký tự"
aliases:
- /vi/php/finding-the-length-of-a-string/
date:                  2024-01-28T22:00:38.813328-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm chiều dài của một chuỗi ký tự"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/finding-the-length-of-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Tìm độ dài của một chuỗi nghĩa là xác định xem nó bao gồm bao nhiêu ký tự. Các lập trình viên thường cần thông tin này cho các nhiệm vụ như xác thực nhập liệu, quản lý chuỗi con, hoặc đơn giản chỉ định dạng đầu ra.

## Làm thế nào:

Sử dụng hàm `strlen()` như sau:

```php
<?php
$text = "Hello, world!";
$length = strlen($text);
echo $length; // Đầu ra: 13
?>
```

Nếu bạn chạy đoạn mã này, bạn sẽ thấy `13` trên màn hình của mình vì "Hello, world!" dài 13 ký tự, bao gồm cả khoảng trống và dấu chấm than.

## Sâu hơn

Hàm `strlen()` đã là một phần của PHP từ những phiên bản đầu tiên. Nó đơn giản và hoạt động dựa trên số byte, thường tương đương với số lượng ký tự trong chuỗi không cần xem xét mã hóa đặc biệt.

Tuy nhiên, với sự quốc tế hóa của các ứng dụng web, việc đối phó với nhiều ngôn ngữ và mã hóa ký tự trở nên phổ biến. Ví dụ, các ký tự trong UTF-8 có thể sử dụng nhiều hơn một byte. Đó là nơi `mb_strlen()` xuất hiện:

```php
<?php
// Một chuỗi với các ký tự đa byte
$multibyteText = "こんにちは";
$length = mb_strlen($multibyteText, "UTF-8");
echo $length; // Đầu ra: 5
?>
```

Năm ký tự, nhưng nhiều byte hơn. Hàm `mb_strlen()` tôn trọng mã hóa ký tự, đảm bảo kiểm tra độ dài chính xác cho các chuỗi đa byte.

`strlen()` nhanh và phù hợp với các bộ ký tự một byte. `mb_strlen()`, mặc dù hơi chậm hơn do cần xử lý mã hóa phức tạp hơn, là cần thiết khi làm việc với văn bản quốc tế hóa.

## Xem thêm

- [Tài liệu chính thức PHP `strlen()`](https://www.php.net/manual/en/function.strlen.php)
- [Tài liệu chính thức PHP `mb_strlen()`](https://www.php.net/manual/en/function.mb-strlen.php)
- [Phần mở rộng Chuỗi Đa Byte PHP](https://www.php.net/manual/en/book.mbstring.php)
