---
title:                "Gửi một yêu cầu HTTP với xác thực cơ bản"
aliases:
- /vi/php/sending-an-http-request-with-basic-authentication/
date:                  2024-01-28T22:08:29.747843-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gửi một yêu cầu HTTP với xác thực cơ bản"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Gì và Tại sao?

Gửi một yêu cầu HTTP với xác thực cơ bản bao gồm việc thêm tên đăng nhập và mật khẩu để truy cập vào một tài nguyên trên máy chủ. Các lập trình viên sử dụng phương pháp này bởi vì một số API và dịch vụ web yêu cầu xác thực để đảm bảo chỉ có người dùng được ủy quyền mới có thể truy cập dữ liệu của họ.

## Cách thực hiện:

Dưới đây là cách đơn giản để gửi một yêu cầu HTTP với xác thực cơ bản sử dụng cURL trong PHP:

```PHP
<?php
$url = 'https://api.example.com/data';
$username = 'your_username';
$password = 'your_password';

$ch = curl_init($url);
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($ch, CURLOPT_USERPWD, "$username:$password");
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);

$response = curl_exec($ch);
curl_close($ch);

echo $response;
?>
```

Kết quả mẫu:

``` 
{
  "authenticated": true,
  "data": "Một số dữ liệu bảo mật"
}
```

## Tìm hiểu kỹ

Xác thực HTTP cơ bản đã được sử dụng từ những ngày đầu của web. Đây không phải là lựa chọn an ninh tốt nhất xung quanh (vì thông tin xác thực được gửi dưới dạng mã hóa base64, dễ dàng bị giải mã), nhưng nó dễ dàng thực hiện cho việc kiểm soát truy cập nhanh chóng.

Giả sử an ninh là một vấn đề (và nó nên được), bạn sẽ chuyển sang các phương pháp mạnh mẽ hơn như OAuth, JWT hoặc khóa API. Tuy nhiên, xác thực cơ bản vẫn tồn tại một phần do các hệ thống di sản và một phần cho các hệ thống nội bộ nơi bạn kiểm soát truy cập một cách chặt chẽ.

Trong PHP, cURL được sử dụng rộng rãi để thực hiện các yêu cầu HTTP, nhưng cũng tồn tại các lựa chọn khác như `file_get_contents` hoặc Guzzle (một khách hàng HTTP PHP). Khi sử dụng `file_get_contents`, một ngữ cảnh với tiêu đề phù hợp phải được tạo ra:

```PHP
<?php
$context = stream_context_create([
    'http' => [
        'header' => "Authorization: Basic " . base64_encode("$username:$password")
    ]
]);

$response = file_get_contents($url, false, $context);

echo $response;
?>
```

Việc chọn công cụ phù hợp phụ thuộc vào nhu cầu của dự án bạn và mức độ kiểm soát và chức năng bạn mong muốn.

## Xem thêm

Để tìm hiểu sâu hơn và mở rộng kiến thức, hãy xem những tài liệu sau:

- [Tài liệu cURL](https://www.php.net/manual/en/book.curl.php)
- [Tài liệu Guzzle](http://docs.guzzlephp.org/en/stable/)
- [Hàm `file_get_contents` trong PHP](https://www.php.net/manual/en/function.file-get-contents.php)
- [Xác thực HTTP với PHP](https://www.php.net/manual/en/features.http-auth.php)
