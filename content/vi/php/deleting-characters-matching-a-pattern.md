---
title:                "Xóa các ký tự phù hợp với một mẫu"
date:                  2024-01-28T21:59:36.728391-07:00
model:                 gpt-4-0125-preview
simple_title:         "Xóa các ký tự phù hợp với một mẫu"

category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì & Tại sao?

Xóa các ký tự khớp với một mẫu trong PHP là việc tìm các chuỗi ký tự cụ thể trong các chuỗi và loại bỏ chúng. Các lập trình viên thực hiện điều này để làm sạch dữ liệu, định dạng đầu ra, hoặc thao tác chuỗi để đáp ứng các tiêu chí cụ thể, như loại bỏ các ký tự không phải chữ số và chữ cái từ đầu vào của người dùng vì lý do an ninh.

## Làm thế nào:

PHP sử dụng hàm `preg_replace` để xóa các ký tự khớp với một mẫu sử dụng biểu thức chính quy. Dưới đây là cách loại bỏ chữ số khỏi một chuỗi:

```PHP
<?php
$text = "Năm 2023!";
$pattern = '/\d+/'; // Mẫu để khớp tất cả các chữ số
$result = preg_replace($pattern, '', $text);
echo $result; // Xuất ra: Năm !
?>
```

Và đây là cách loại bỏ khoảng trắng:

```PHP
<?php
$text = "Quá    nhiều      khoảng trắng!";
$pattern = '/\s+/'; // Mẫu để khớp tất cả khoảng trắng
$result = preg_replace($pattern, ' ', $text);
echo $result; // Xuất ra: Quá nhiều khoảng trắng!
?>
```

## Sâu hơn

Việc xóa các ký tự bằng cách khớp mẫu không phải là mới. Hàm `preg_replace` của PHP, làm nền tảng cho chức năng này, sử dụng biểu thức chính quy tương thích với Perl, một yếu tố cơ bản của việc xử lý văn bản kể từ khi Perl nổi lên vào cuối những năm 80. Các phương thức khác của `preg_replace` bao gồm `str_replace` cho các thay thế đơn giản và `trim`, `ltrim`, và `rtrim` để loại bỏ khoảng trắng khỏi chuỗi. Đối với việc xóa mẫu tinh vi hơn, `preg_replace_callback` có thể được sử dụng để kiểm soát thêm trong quá trình thay thế.

Thật hữu ích khi biết rằng PREG trong `preg_replace` đại diện cho Perl Regular Expressions, biểu thị việc PHP sử dụng cú pháp mẫu của Perl. Dưới đây là cách phân tích:

- `\d` khớp với bất kỳ chữ số nào. Thêm `+` có nghĩa là "một hoặc nhiều hơn" của thành phần đứng trước (chữ số, trong trường hợp này).
- `\s` tìm bất kỳ khoảng trắng nào. Giống như số, `+` sau `\s` nhắm vào những khoảng trống dài.

Việc lựa chọn giữa `preg_replace` và các phương pháp thay thế của nó phụ thuộc vào việc bạn đang làm gì. Sử dụng `preg_replace` cho các mẫu phức tạp và `str_replace` khi xử lý các thay thế đơn giản, trực tiếp.

Nhớ rằng, việc sử dụng không đúng cách biểu thức chính quy có thể dẫn đến việc tạo ra mã không hiệu quả. Luôn đo lường và sử dụng biểu thức chính quy một cách thông minh.

## Xem Thêm

Để biết thêm về các hàm chuỗi và khớp mẫu trong PHP:
- [Tài liệu PHP — preg_replace](https://www.php.net/manual/en/function.preg-replace.php)
- [Tài liệu PHP — Biểu thức chính quy (Tương thích với Perl)](https://www.php.net/manual/en/book.pcre.php)
- [Tài liệu PHP — str_replace](https://www.php.net/manual/en/function.str-replace.php)
- [Tài liệu PHP — Hàm Chuỗi](https://www.php.net/manual/en/ref.strings.php)

Những liên kết này dẫn đến tài liệu chính thức của PHP nơi bạn có thể tìm hiểu sâu hơn về việc thao tác chuỗi và khớp mẫu.
