---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:00.374971-07:00
description: "L\xE0m th\u1EBF n\xE0o: PHP cung c\u1EA5p h\u1ED7 tr\u1EE3 s\u1EB5n\
  \ c\xF3 cho s\u1ED1 ph\u1EE9c s\u1EED d\u1EE5ng ti\u1EC7n \xEDch m\u1EDF r\u1ED9\
  ng `ext-intl` v\u1EDBi l\u1EDBp `NumberFormatter`. D\u01B0\u1EDBi \u0111\xE2y l\xE0\
  \ m\u1ED9t v\xED d\u1EE5."
lastmod: '2024-03-13T22:44:36.758143-06:00'
model: gpt-4-0125-preview
summary: "PHP cung c\u1EA5p h\u1ED7 tr\u1EE3 s\u1EB5n c\xF3 cho s\u1ED1 ph\u1EE9c\
  \ s\u1EED d\u1EE5ng ti\u1EC7n \xEDch m\u1EDF r\u1ED9ng `ext-intl` v\u1EDBi l\u1EDB\
  p `NumberFormatter`."
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
weight: 14
---

## Làm thế nào:
PHP cung cấp hỗ trợ sẵn có cho số phức sử dụng tiện ích mở rộng `ext-intl` với lớp `NumberFormatter`. Dưới đây là một ví dụ:

```php
// Đảm bảo rằng tiện ích mở rộng intl đã được tải
if (!extension_loaded('intl')) {
    die("Tiện ích mở rộng intl không được kích hoạt. Hãy kích hoạt nó để chạy code này.");
}

function addComplexNumbers($a, $b) {
    // Sử dụng NumberFormatter để phân tích và định dạng số phức
    $formatter = new NumberFormatter('en_US', NumberFormatter::PATTERN_RULEBASED, 'i = -1;');

    // Phân tích số phức từ chuỗi
    $numA = $formatter->parse($a, NumberFormatter::TYPE_DOUBLE);
    $numB = $formatter->parse($b, NumberFormatter::TYPE_DOUBLE);

    // Thực hiện phép cộng
    $sum = $numA + $numB;

    // Định dạng kết quả dưới dạng một số phức
    return $formatter->format($sum);
}

echo addComplexNumbers('5+3i', '2+7i'); // Đầu ra: 7+10i
```

## Sâu hơn
Trước `ext-intl`, PHP không hỗ trợ số phức một cách tự nhiên. Các nhà phát triển sử dụng hàm hoặc thư viện lớp tùy chỉnh để xử lý số phức. Các thao tác phức tạp có thể trở nên mệt mỏi và dễ mắc lỗi, nhưng `ext-intl` cung cấp một cách quốc tế hóa để trình bày và phân tích số phức phù hợp với thư viện ICU.

Tuy nhiên, cho các thao tác toán học nặng hơn, một số người có thể sử dụng các thư viện bên ngoài được viết bằng ngôn ngữ thân thiện với toán học hơn (như C hay Python) và giao tiếp với chúng thông qua PHP. Về cài đặt, `ext-intl` xử lý nó ẩn dưới, đảm bảo tính toán chính xác trong khi trừu tượng hóa độ phức tạp khỏi nhà phát triển.

Về mặt lịch sử, số phức thường bị xem xét không tốt khi được gọi là 'ảo', nhưng chúng đã trở nên cơ bản trong nhiều lĩnh vực khoa học và toán học, tiết lộ nhiều hơn về ý nghĩa thực sự trong thế giới thực so với tên gọi 'ảo' từng gợi ý.

## Xem thêm
- [Tài liệu PHP về NumberFormatter](https://www.php.net/manual/en/class.numberformatter.php)
- [Wikipedia về số phức](https://en.wikipedia.org/wiki/Complex_number)
- [PHP: The Right Way - Làm việc với Các Kiểu Dữ liệu](https://phptherightway.com/#data_types)
