---
title:                "Tìm kiếm và thay thế văn bản"
date:                  2024-01-28T22:07:52.171316-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tìm kiếm và thay thế văn bản"

category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/searching-and-replacing-text.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Tìm và Thay thế Văn Bản Với PHP

### Điều gì và Tại sao?

Tìm và thay thế văn bản là cách bạn tìm các chuỗi cụ thể trong nội dung và thay thế chúng bằng thứ khác. Lập trình viên làm điều này để cập nhật dữ liệu, sửa lỗi hoặc thay đổi văn bản hàng loạt mà không cần chỉnh sửa thủ công.

### Cách thực hiện:

Dưới đây là cách nhanh chóng để thay thế 'cat' bằng 'dog' trong một câu sử dụng PHP:

```PHP
<?php
$text = 'The quick brown fox jumps over the lazy cat';
$replacedText = str_replace('cat', 'dog', $text);

echo $replacedText;
?>
```

Kết quả hiển thị:

```
The quick brown fox jumps over the lazy dog
```

Giờ, giả sử chúng ta đang xử lý thay thế không phân biệt chữ hoa chữ thường:

```PHP
<?php
$text = 'Catapults are CATegorically amazing!';
$replacedText = str_ireplace('cat', 'dog', $text);

echo $replacedText;
?>
```

Kết quả hiển thị:

```
Dogapults are DOGegorically amazing!
```

### Sâu hơn nữa:

Chức năng tìm kiếm và thay thế đã tồn tại từ những ngày đầu tiên của máy tính - nghĩ về `sed` trong Unix. Trong PHP, `str_replace` và `str_ireplace` là những lựa chọn của bạn cho việc tìm kiếm và thay thế đơn giản. `str_replace` phân biệt chữ hoa chữ thường, trong khi `str_ireplace` thì không.

Chúng hoạt động như thế nào? Cơ bản, cả hai hàm đều kiểm tra từng phần của chuỗi, tìm kiếm các trùng khớp và thay thế chúng. Chúng còn xử lý mảng, vì vậy bạn có thể tìm kiếm và thay thế nhiều mẫu trong một lần chạy.

Bây giờ, nếu bạn cần nhiều kiểm soát hơn, như phù hợp với mẫu, bạn sẽ muốn sử dụng `preg_replace`. Hàm này sử dụng biểu thức chính quy, cung cấp nhiều linh hoạt và chính xác hơn:

```PHP
<?php
$text = 'The quick brown fox jumps over the lazy cat 7 times.';
$replacedText = preg_replace('/\bcat\b/i', 'dog', $text);

echo $replacedText;
?>
```

Kết quả hiển thị:

```
The quick brown fox jumps over the lazy dog 7 times.
```

Lần này, 'cat' được thay thế bằng 'dog', bỏ qua chữ hoa chữ thường (`/i` chỉ báo), và chỉ khớp với từ nguyên (`\b` biên giới từ).

### Xem thêm:

- Tài liệu chính thức của PHP về str_replace: https://www.php.net/manual/en/function.str-replace.php
- Tài liệu chính thức của PHP về str_ireplace: https://www.php.net/manual/en/function.str-ireplace.php
- Tài liệu chính thức của PHP về preg_replace: https://www.php.net/manual/en/function.preg-replace.php
- Hướng dẫn về Biểu thức Chính quy: https://www.regular-expressions.info/
- Biên tập viên dòng lệnh `sed` trong Unix cho việc lọc và biến đổi văn bản: http://www.grymoire.com/Unix/Sed.html
