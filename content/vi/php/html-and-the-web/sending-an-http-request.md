---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:27.205311-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: PHP c\xF3 m\u1ED9t c\xE1ch g\u1ECDn l\xE0\
  nh \u0111\u1EC3 x\u1EED l\xFD y\xEAu c\u1EA7u HTTP v\u1EDBi th\u01B0 vi\u1EC7n `cURL`.\
  \ Nh\u01B0ng c\xE1ch m\u1EDBi m\u1EBB h\u01A1n l\xE0 s\u1EED d\u1EE5ng `file_get_contents`\
  \ cho c\xE1c y\xEAu c\u1EA7u\u2026"
lastmod: '2024-03-13T22:44:36.762162-06:00'
model: gpt-4-0125-preview
summary: "PHP c\xF3 m\u1ED9t c\xE1ch g\u1ECDn l\xE0nh \u0111\u1EC3 x\u1EED l\xFD y\xEA\
  u c\u1EA7u HTTP v\u1EDBi th\u01B0 vi\u1EC7n `cURL`."
title: "G\u1EEDi m\u1ED9t y\xEAu c\u1EA7u HTTP"
weight: 44
---

## Cách thực hiện:
PHP có một cách gọn lành để xử lý yêu cầu HTTP với thư viện `cURL`. Nhưng cách mới mẻ hơn là sử dụng `file_get_contents` cho các yêu cầu GET đơn giản, hoặc `stream_context_create` cho các yêu cầu POST. Dưới đây là cái nhìn nhanh về cả hai.

### Yêu cầu GET với file_get_contents():
```php
// URL bạn muốn truy cập
$url = "http://example.com/api";

// Sử dụng file_get_contents để thực hiện một yêu cầu GET
$response = file_get_contents($url);

// Xuất kết quả để xem bạn nhận được gì
var_dump($response);
```

### Yêu cầu POST với stream_context_create():
```php
// URL bạn muốn đăng lên
$url = "http://example.com/api";

// Dữ liệu bạn gửi
$data = http_build_query([
    'foo' => 'bar',
    'baz' => 'qux',
]);

// Tùy chọn context luồng
$options = [
    'http' => [
        'header'  => "Content-type: application/x-www-form-urlencoded\r\n",
        'method'  => 'POST',
        'content' => $data,
    ],
];

// Tạo một context luồng
$context  = stream_context_create($options);

// Thực hiện yêu cầu POST và đặt phản hồi vào một biến
$result = file_get_contents($url, false, $context);

// Xem bạn đã nhận được gì
var_dump($result);
```

## Khám phá sâu hơn
Ngày xưa, `fsockopen()` là phương pháp được ưa chuộng để thực hiện yêu cầu HTTP với PHP. Nó có vẻ cồng kềnh, nhưng vẫn đạt được mục tiêu. Rồi `cURL` xuất hiện, vẫn mạnh mẽ và được sử dụng rộng rãi, đặc biệt là cho các thao tác phức tạp. Nhưng đôi khi, bạn không cần một cái cưa máy chỉ để cắt một sợi dây. Đó là nơi mà `file_get_contents()` và `stream_context_create()` tỏa sáng.

Một điều chính về `file_get_contents()` là sự đơn giản của nó. Hoàn hảo cho những yêu cầu GET đơn giản. Nhưng nếu bạn cần gửi dữ liệu bằng POST thì sao? Nhập cuộc `stream_context_create()`. Viên ngọc nhỏ này cho phép bạn tinh chỉnh các yêu cầu HTTP của mình với tiêu đề, phương pháp và nhiều hơn nữa.

Bên trong, `file_get_contents()` và `stream_context_create()` sử dụng bọc luồng của PHP. Những cái này thay thế cho các thao tác socket cấp thấp được xử lý bởi `fsockopen()`.

Một nhược điểm? Xử lý lỗi có thể khó khăn hơn. Nếu có điều gì đó không đúng, các hàm này ít khoan dung hơn `cURL`. Nếu bạn cần thông tin phản hồi chi tiết hoặc phải xử lý các nhiệm vụ HTTP phức tạp, hãy cân nhắc việc sử dụng `cURL`.

## Xem thêm
- Tài liệu PHP chính thức của cURL: [https://www.php.net/manual/en/book.curl.php](https://www.php.net/manual/en/book.curl.php)
- Các ngữ cảnh luồng PHP: [https://www.php.net/manual/en/context.php](https://www.php.net/manual/en/context.php)
- Tùy chọn ngữ cảnh HTTP: [https://www.php.net/manual/en/context.http.php](https://www.php.net/manual/en/context.http.php)
