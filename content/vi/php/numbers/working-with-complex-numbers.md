---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:00.374971-07:00
description: "S\u1ED1 ph\u1EE9c c\xF3 m\u1ED9t ph\u1EA7n th\u1EF1c v\xE0 m\u1ED9t\
  \ ph\u1EA7n \u1EA3o, th\u01B0\u1EDDng \u0111\u01B0\u1EE3c vi\u1EBFt d\u01B0\u1EDB\
  i d\u1EA1ng `a + bi`. Ch\xFAng r\u1EA5t quan tr\u1ECDng trong to\xE1n h\u1ECDc n\xE2\
  ng cao, v\u1EADt l\xFD, k\u1EF9 thu\u1EADt, v\xE0 m\u1ED9t s\u1ED1\u2026"
lastmod: 2024-02-19 22:04:55.940615
model: gpt-4-0125-preview
summary: "S\u1ED1 ph\u1EE9c c\xF3 m\u1ED9t ph\u1EA7n th\u1EF1c v\xE0 m\u1ED9t ph\u1EA7\
  n \u1EA3o, th\u01B0\u1EDDng \u0111\u01B0\u1EE3c vi\u1EBFt d\u01B0\u1EDBi d\u1EA1\
  ng `a + bi`. Ch\xFAng r\u1EA5t quan tr\u1ECDng trong to\xE1n h\u1ECDc n\xE2ng cao,\
  \ v\u1EADt l\xFD, k\u1EF9 thu\u1EADt, v\xE0 m\u1ED9t s\u1ED1\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi s\u1ED1 ph\u1EE9c"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
Số phức có một phần thực và một phần ảo, thường được viết dưới dạng `a + bi`. Chúng rất quan trọng trong toán học nâng cao, vật lý, kỹ thuật, và một số thuật toán máy tính. Lập trình viên làm việc với chúng để xử lý các phép tính liên quan đến căn bậc hai của số âm và các hàm dao động.

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
