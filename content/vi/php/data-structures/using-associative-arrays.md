---
aliases:
- /vi/php/using-associative-arrays/
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:12:34.144332-07:00
description: "M\u1EA3ng k\u1EBFt h\u1EE3p trong PHP gi\u1ED1ng nh\u01B0 nh\u1EEFng\
  \ danh s\xE1ch n\xE2ng cao, n\u01A1i m\u1ED7i ph\u1EA7n t\u1EED c\xF3 th\u1EC3 \u0111\
  \u01B0\u1EE3c truy c\u1EADp b\u1EB1ng m\u1ED9t kh\xF3a d\u1EC5 hi\u1EC3u thay v\xEC\
  \ ch\u1EC9 l\xE0 s\u1ED1. L\u1EADp tr\xECnh vi\xEAn s\u1EED\u2026"
lastmod: 2024-02-18 23:08:50.782674
model: gpt-4-0125-preview
summary: "M\u1EA3ng k\u1EBFt h\u1EE3p trong PHP gi\u1ED1ng nh\u01B0 nh\u1EEFng danh\
  \ s\xE1ch n\xE2ng cao, n\u01A1i m\u1ED7i ph\u1EA7n t\u1EED c\xF3 th\u1EC3 \u0111\
  \u01B0\u1EE3c truy c\u1EADp b\u1EB1ng m\u1ED9t kh\xF3a d\u1EC5 hi\u1EC3u thay v\xEC\
  \ ch\u1EC9 l\xE0 s\u1ED1. L\u1EADp tr\xECnh vi\xEAn s\u1EED\u2026"
title: "S\u1EED d\u1EE5ng m\u1EA3ng li\xEAn k\u1EBFt"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Mảng kết hợp trong PHP giống như những danh sách nâng cao, nơi mỗi phần tử có thể được truy cập bằng một khóa dễ hiểu thay vì chỉ là số. Lập trình viên sử dụng chúng để lưu trữ và thao tác dữ liệu một cách trực quan hơn, cho phép mã dễ đọc và dễ bảo trì hơn.

## Làm thế nào:

Trong PHP, việc tạo và sử dụng mảng kết hợp rất đơn giản. Dưới đây là một bản tóm tắt nhanh:

```PHP
<?php
// Tạo một mảng kết hợp
$person = array(
    "name" => "John Doe",
    "age" => 30,
    "email" => "john@example.com"
);

// Hoặc, cú pháp mảng ngắn
$person = [
    "name" => "John Doe",
    "age" => 30,
    "email" => "john@example.com"
];

// Truy cập giá trị bằng khóa
echo "Tên: " . $person["name"] . "\n";
echo "Tuổi: " . $person["age"] . "\n";
echo "Email: " . $person["email"] . "\n";

// Chỉnh sửa một giá trị
$person["age"] = 31;

// Thêm một cặp khóa-giá trị mới
$person["country"] = "Mỹ";

// Lặp qua một mảng kết hợp
foreach ($person as $key => $value) {
    echo $key . ": " . $value . "\n";
}

// Kết quả
// Tên: John Doe
// Tuổi: 31
// Email: john@example.com
// quốc gia: Mỹ
?>
```

Chú ý là khóa có thể là bất kỳ chuỗi nào, cho phép bạn truy cập các phần tử bằng những khóa này thay vì chỉ dùng các chỉ số số, có thể kém ý nghĩa và khó nhớ hơn.

## Nghiên cứu sâu

Mảng kết hợp trong PHP được triển khai nội bộ sử dụng bảng băm, cung cấp việc truy cập rất nhanh đến các phần tử bằng khóa, làm cho chúng rất hiệu quả cho nhiều nhiệm vụ. Hiệu quả này, kết hợp với sự dễ sử dụng, làm cho mảng kết hợp trở thành một trụ cột của lập trình PHP.

Trong lịch sử, mảng của PHP (cả chỉ mục và mảng kết hợp) đã rất linh hoạt, cho phép chúng phục vụ như danh sách, ngăn xếp, hàng đợi, và nhiều hơn nữa. Tuy nhiên, tính linh hoạt này đôi khi có thể dẫn đến mã kém hiệu quả nếu không được sử dụng một cách cẩn thận.

Gần đây, với những cải tiến trong lập trình hướng đối tượng trong PHP, một số nhà phát triển thích sử dụng đối tượng cho dữ liệu có cấu trúc, đặc biệt là cho các bộ dữ liệu phức tạp hoặc có liên quan với nhau. Sử dụng các lớp có thể cung cấp khả năng đóng gói và trừu tượng hóa tốt hơn, làm cho mã dễ kiểm tra hơn và làm rõ ý định. Tuy nhiên, cho việc lưu trữ đơn giản giá trị khóa và các tình huống thao tác dữ liệu đơn giản, mảng kết hợp vẫn là một lựa chọn xuất sắc do tính đơn giản và cú pháp trực quan của chúng.
