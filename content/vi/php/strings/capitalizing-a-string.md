---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:04.331367-07:00
description: "L\xE0m th\u1EBF n\xE0o: Trong PHP, ch\xFAng ta vi\u1EBFt hoa chu\u1ED7\
  i b\u1EB1ng h\xE0m `ucwords()` cho c\xE1c ti\xEAu \u0111\u1EC1 \u0111\u1EA7y \u0111\
  \u1EE7 ho\u1EB7c `ucfirst()` cho m\u1ED9t d\xF2ng \u0111\u01A1n ho\u1EB7c c\xE2\
  u."
lastmod: '2024-03-13T22:44:36.744288-06:00'
model: gpt-4-0125-preview
summary: "Trong PHP, ch\xFAng ta vi\u1EBFt hoa chu\u1ED7i b\u1EB1ng h\xE0m `ucwords()`\
  \ cho c\xE1c ti\xEAu \u0111\u1EC1 \u0111\u1EA7y \u0111\u1EE7 ho\u1EB7c `ucfirst()`\
  \ cho m\u1ED9t d\xF2ng \u0111\u01A1n ho\u1EB7c c\xE2u."
title: "Vi\u1EBFt hoa m\u1ED9t chu\u1ED7i"
weight: 2
---

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
