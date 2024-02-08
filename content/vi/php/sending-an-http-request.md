---
title:                "Gửi một yêu cầu HTTP"
aliases:
- vi/php/sending-an-http-request.md
date:                  2024-01-28T22:09:27.205311-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gửi một yêu cầu HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/sending-an-http-request.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Cái gì & Tại sao?

Gửi một yêu cầu HTTP là quy trình mà chương trình yêu cầu dữ liệu từ máy chủ. Lập trình viên thực hiện việc này để tương tác với dịch vụ web, API, hoặc đơn giản là để tải nội dung trang web.

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
