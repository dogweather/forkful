---
title:                "Viết hoa một chuỗi"
date:                  2024-01-28T21:56:04.331367-07:00
model:                 gpt-4-0125-preview
simple_title:         "Viết hoa một chuỗi"

category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Việc viết hoa một chuỗi có nghĩa là viết hoa chữ cái đầu tiên của mỗi từ. Các lập trình viên viết hoa chuỗi để đảm bảo tính nhất quán về mặt hình thức, thương hiệu, hoặc thiết kế trải nghiệm người dùng.

## Làm thế nào:
Trong PHP, chúng ta viết hoa chuỗi bằng hàm `ucwords()` cho các tiêu đề đầy đủ hoặc `ucfirst()` cho một dòng đơn hoặc câu.

```php
<?php
$lowercase_title = "the quick brown fox jumps over the lazy dog";
$capitalized_title = ucwords($lowercase_title);

echo $capitalized_title; // Xuất ra: The Quick Brown Fox Jumps Over The Lazy Dog

$sentence = "an example sentence.";
$capitalized_sentence = ucfirst($sentence);

echo $capitalized_sentence; // Xuất ra: An example sentence.
?>
```

## Sâu hơn
Việc viết hoa chuỗi không phải là một khái niệm mới. Trong thế giới in ấn, quy tắc viết hoa tiêu đề là một quy ước chuẩn. Trong PHP, `ucwords` và `ucfirst` đã tồn tại một thời gian, hỗ trợ các quy ước như vậy một cách số hóa. Hàm `mb_convert_case` của PHP cho phép thao tác phức tạp hơn, như `MB_CASE_TITLE`, đặc biệt hữu ích cho các chuỗi đa byte (không phải ASCII).

Các phương án thay thế cho `ucwords` bao gồm `strtoupper`, chuyển toàn bộ chuỗi thành chữ hoa, và `strtolower` chuyển chuỗi thành chữ thường. Hãy lưu ý về địa phương: một số ngôn ngữ có quy tắc viết hoa độc đáo.

Về mặt hiện thực, `ucwords` áp dụng chữ hoa cho ký tự đầu tiên sau mọi khoảng trắng, không chỉ là dấu cách. Điều này có nghĩa là dòng mới, tab, v.v., đều kích hoạt việc viết hoa.

## Tham Khảo Thêm
Để biết thêm thông tin, kiểm tra tại:

- Sổ tay PHP về `ucwords()`: https://www.php.net/manual/en/function.ucwords.php
- Sổ tay PHP về `ucfirst()`: https://www.php.net/manual/en/function.ucfirst.php
- Sổ tay PHP về `mb_convert_case()`: https://www.php.net/manual/en/function.mb-convert-case.php
- Các hàm chuỗi PHP: https://www.php.net/manual/en/ref.strings.php
