---
title:                "Làm việc với JSON"
date:                  2024-01-28T22:10:44.003807-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với JSON"

category:             "PHP"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/working-with-json.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?
JSON (JavaScript Object Notation) là một định dạng dữ liệu nhẹ dùng để trao đổi dữ liệu. Lập trình viên sử dụng nó vì dễ đọc/viết và độc lập với ngôn ngữ, làm cho nó trở thành lựa chọn lý tưởng cho APIs và dịch vụ web.

## Làm thế nào:

### Mã hóa một mảng thành JSON
```php
$array = ['foo' => 'bar', 'baz' => 'qux'];
$json = json_encode($array);
echo $json; // {"foo":"bar","baz":"qux"}
```

### Giải mã JSON thành một đối tượng
```php
$json = '{"foo":"bar","baz":"qux"}';
$object = json_decode($json);
echo $object->foo; // bar
```

### Giải mã JSON thành một mảng kết hợp
```php
$json = '{"foo":"bar","baz":"qux"}';
$array = json_decode($json, true);
echo $array['foo']; // bar
```

### Xử lý lỗi JSON
```php
$json = '{"foo":"bar,"baz":"qux"}'; // Lưu ý dấu nháy bị thiếu
$array = json_decode($json, true);

if(json_last_error() != JSON_ERROR_NONE) {
   echo json_last_error_msg(); // Lỗi cú pháp, JSON không hợp lệ
}
```

## Sâu hơn

JSON đã trở thành chuẩn mực de facto cho việc trao đổi dữ liệu web từ đầu những năm 2000, thay thế XML do sự đơn giản của nó. Các lựa chọn thay thế như XML và YAML tồn tại, nhưng độ gọn nhẹ và tốc độ của JSON đã làm cho nó trở thành lựa chọn hàng đầu. Các hàm `json_encode()` và `json_decode()` của PHP tuần tự hóa và giải tuần tự hóa dữ liệu, tương ứng. Kể từ PHP 5.4.0, tùy chọn `JSON_PRETTY_PRINT` làm cho đầu ra dễ đọc hơn, và từ PHP 7.3.0, các nhà phát triển có thể ném `JsonException` để xử lý lỗi, làm cho việc phân tích JSON trở nên mạnh mẽ hơn.

## Xem thêm

- Tài liệu PHP về JSON: https://www.php.net/manual/en/book.json.php
- Trang chủ JSON: http://json.org/
- PHP The Right Way (mục xử lý JSON): https://phptherightway.com/#json
- Composer, một trình quản lý phụ thuộc cho PHP (sử dụng JSON cho thông tin gói): https://getcomposer.org/
