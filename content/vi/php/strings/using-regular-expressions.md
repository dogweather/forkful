---
aliases:
- /vi/php/using-regular-expressions/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:07.953523-07:00
description: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy (regex) l\xE0 c\xE1c m\u1EABu t\xEC\
  m ki\u1EBFm \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng \u0111\u1EC3 kh\u1EDBp c\xE1c\
  \ k\u1EBFt h\u1EE3p k\xFD t\u1EF1 trong chu\u1ED7i. L\u1EADp tr\xECnh vi\xEAn s\u1EED\
  \ d\u1EE5ng ch\xFAng cho c\xE1c nhi\u1EC7m v\u1EE5 nh\u01B0 x\xE1c\u2026"
lastmod: 2024-02-18 23:08:50.779470
model: gpt-4-0125-preview
summary: "Bi\u1EC3u th\u1EE9c ch\xEDnh quy (regex) l\xE0 c\xE1c m\u1EABu t\xECm ki\u1EBF\
  m \u0111\u01B0\u1EE3c s\u1EED d\u1EE5ng \u0111\u1EC3 kh\u1EDBp c\xE1c k\u1EBFt h\u1EE3\
  p k\xFD t\u1EF1 trong chu\u1ED7i. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng ch\xFA\
  ng cho c\xE1c nhi\u1EC7m v\u1EE5 nh\u01B0 x\xE1c\u2026"
title: "S\u1EED d\u1EE5ng bi\u1EC3u th\u1EE9c ch\xEDnh quy"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Biểu thức chính quy (regex) là các mẫu tìm kiếm được sử dụng để khớp các kết hợp ký tự trong chuỗi. Lập trình viên sử dụng chúng cho các nhiệm vụ như xác minh, tìm kiếm và phân tích cú pháp văn bản, bởi vì chúng mạnh mẽ và tiết kiệm thời gian.

## Làm thế nào:
Để sử dụng regex trong PHP, bạn thường sử dụng `preg_match` để tìm kiếm một kết quả khớp, hoặc `preg_replace` cho tìm kiếm và thay thế. Dưới đây là một cái nhìn nhanh:

```php
<?php
$string = "The quick brown fox jumps over the lazy dog.";

// Kiểm tra xem 'quick' có trong chuỗi không
if (preg_match("/quick/", $string)) {
  echo "Tìm thấy khớp!";
} else {
  echo "Không tìm thấy khớp.";
}
// Kết quả: Tìm thấy khớp!

// Thay thế 'brown' bằng 'red'
$replacedString = preg_replace("/brown/", "red", $string);
echo $replacedString;
// Kết quả: The quick red fox jumps over the lazy dog.
?>
```

## Sâu hơn
Biểu thức chính quy đã có từ những năm 1950 và được triển khai mạnh mẽ trong Perl, ảnh hưởng đến nhiều ngôn ngữ khác, bao gồm PHP. Các phương thức thay thế cho regex trong PHP bao gồm các hàm như `strpos()` để tìm chuỗi con hoặc `str_replace()` để thay thế văn bản. Thư viện PCRE (Perl Compatible Regular Expressions) là thứ mà PHP sử dụng phía sau cho các chức năng regex, cung cấp khả năng khớp mẫu mạnh mẽ và phong phú.

## Xem thêm
- [Tài liệu chính thức về PCRE của PHP](https://www.php.net/manual/en/book.pcre.php)
- [Regular-Expressions.info](https://www.regular-expressions.info/) - để hiểu biết kỹ lưỡng về regex.
- [Regex101](https://regex101.com/) - để thử nghiệm và gỡ lỗi các mẫu regex của bạn.
