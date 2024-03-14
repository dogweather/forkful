---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:51.792830-07:00
description: ''
lastmod: '2024-03-13T22:44:36.794108-06:00'
model: gpt-4-0125-preview
summary: ''
title: "L\xE0m vi\u1EC7c v\u1EDBi YAML"
---

{{< edit_this_page >}}

## Làm việc với YAML bằng PHP

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
