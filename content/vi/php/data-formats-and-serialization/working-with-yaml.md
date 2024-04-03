---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:51.792830-07:00
description: "C\xE1i g\xEC & T\u1EA1i sao? YAML l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1\
  ng tu\u1EA7n t\u1EF1 h\xF3a d\u1EEF li\u1EC7u th\xE2n thi\u1EC7n v\u1EDBi con ng\u01B0\
  \u1EDDi. C\xE1c l\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng n\xF3 cho c\xE1c t\u1EC7\
  p c\u1EA5u h\xECnh, trao \u0111\u1ED5i d\u1EEF li\u1EC7u\u2026"
lastmod: '2024-03-13T22:44:36.794108-06:00'
model: gpt-4-0125-preview
summary: "YAML l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1ng tu\u1EA7n t\u1EF1 h\xF3a d\u1EEF\
  \ li\u1EC7u th\xE2n thi\u1EC7n v\u1EDBi con ng\u01B0\u1EDDi."
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
weight: 41
---

## Cái gì & Tại sao?
YAML là một định dạng tuần tự hóa dữ liệu thân thiện với con người. Các lập trình viên sử dụng nó cho các tệp cấu hình, trao đổi dữ liệu và lưu trữ do khả năng đọc và đơn giản của nó.

## Cách thực hiện:
Để bắt đầu với YAML trong PHP, bạn cần phải có tiện ích mở rộng `yaml`. Dưới đây là cách nhanh chóng để bắt đầu:

**Cài đặt tiện ích mở rộng YAML** (nếu chưa được cài đặt):
```bash
pecl install yaml
```

**Tải tiện ích mở rộng**:
Đảm bảo `php.ini` của bạn bao gồm:
```ini
extension=yaml
```

**Phân tích cú pháp YAML**: 
```php
<?php
$yamlString = "
settings:
  database: MySQL
  host: localhost";

$array = yaml_parse($yamlString);

print_r($array);
```
**Kết quả mẫu**:
```
Array
(
    [settings] => Array
        (
            [database] => MySQL
            [host] => localhost
        )
)
```

**Tạo YAML**:
```php
<?php
$array = [
  'settings' => [
    'database' => 'MySQL',
    'host' => 'localhost'
  ]
];

$yamlString = yaml_emit($array);
echo $yamlString;
```
**Kết quả mẫu**:
```
settings:
  database: MySQL
  host: localhost
```

## Sâu hơn:
YAML, viết tắt của "YAML Ain't Markup Language," tập trung vào dữ liệu và cấu trúc dữ liệu, và nó nổi bật ở nơi mà các ngôn ngữ như XML có thể quá phức tạp. Nó được phát hành lần đầu tiên vào năm 2001. Các lựa chọn thay thế bao gồm JSON và XML; YAML thường được ưu tiên vì khả năng đọc của con người. Tiện ích mở rộng `yaml` của PHP kết nối với thư viện `libyaml`, đảm bảo việc phân tích cú pháp và phát ra nhanh chóng.

## Xem thêm:
- Tài liệu mở rộng chính thức của PHP về YAML: https://www.php.net/manual/en/book.yaml.php
- Trang web chính thức của YAML: https://yaml.org
- So sánh các định dạng tuần tự hóa dữ liệu: https://en.wikipedia.org/wiki/Comparison_of_data_serialization_formats
