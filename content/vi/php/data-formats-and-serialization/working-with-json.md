---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:10:44.003807-07:00
description: "JSON (JavaScript Object Notation) l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1\
  ng d\u1EEF li\u1EC7u nh\u1EB9 d\xF9ng \u0111\u1EC3 trao \u0111\u1ED5i d\u1EEF li\u1EC7\
  u. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng n\xF3 v\xEC d\u1EC5 \u0111\u1ECD\
  c/vi\u1EBFt v\xE0 \u0111\u1ED9c l\u1EADp v\u1EDBi ng\xF4n ng\u1EEF,\u2026"
lastmod: '2024-02-25T18:49:35.138989-07:00'
model: gpt-4-0125-preview
summary: "JSON (JavaScript Object Notation) l\xE0 m\u1ED9t \u0111\u1ECBnh d\u1EA1\
  ng d\u1EEF li\u1EC7u nh\u1EB9 d\xF9ng \u0111\u1EC3 trao \u0111\u1ED5i d\u1EEF li\u1EC7\
  u. L\u1EADp tr\xECnh vi\xEAn s\u1EED d\u1EE5ng n\xF3 v\xEC d\u1EC5 \u0111\u1ECD\
  c/vi\u1EBFt v\xE0 \u0111\u1ED9c l\u1EADp v\u1EDBi ng\xF4n ng\u1EEF,\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi JSON"
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
