---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:02.291102-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: PHP l\xE0m cho vi\u1EC7c t\u1EA3i trang\
  \ web tr\u1EDF n\xEAn kh\xE1 \u0111\u01A1n gi\u1EA3n. D\u01B0\u1EDBi \u0111\xE2\
  y l\xE0 m\u1ED9t v\xED d\u1EE5 \u0111\u01A1n gi\u1EA3n s\u1EED d\u1EE5ng `file_get_contents()`."
lastmod: '2024-03-13T22:44:36.764713-06:00'
model: gpt-4-0125-preview
summary: "PHP l\xE0m cho vi\u1EC7c t\u1EA3i trang web tr\u1EDF n\xEAn kh\xE1 \u0111\
  \u01A1n gi\u1EA3n."
title: "T\u1EA3i trang web"
weight: 42
---

## Cách thực hiện:
PHP làm cho việc tải trang web trở nên khá đơn giản. Dưới đây là một ví dụ đơn giản sử dụng `file_get_contents()`:

```php
<?php
$url = "http://example.com";
$pageContent = file_get_contents($url);

if ($pageContent !== false) {
    echo "Tải trang thành công.\n";
    // Thực hiện công việc với $pageContent
} else {
    echo "Tải trang thất bại.\n";
}
?>
```

Và nếu bạn cần nhiều kiểm soát hơn hoặc muốn xử lý các tiêu đề HTTP, cookie, hoặc yêu cầu POST, bạn có thể sử dụng `cURL` một cách tinh vi:

```php
<?php
$url = "http://example.com";
$ch = curl_init($url);

curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
$pageContent = curl_exec($ch);

if (curl_errno($ch)) {
    echo "Lỗi: " . curl_error($ch) . "\n";
} else {
    echo "Tải trang thành công.\n";
    // Thực hiện công việc với $pageContent
}

curl_close($ch);
?>
```

Kết quả mẫu có thể là:
```
Tải trang thành công.
```

## Tìm hiểu kỹ
Việc tải trang web là một phương pháp đã tồn tại từ khi web ra đời. Ban đầu, để tương tác với các trang web, bạn sẽ sử dụng các công cụ dòng lệnh như `wget` hoặc `curl`. Tuy nhiên, khi PHP phát triển, các hàm đã làm cho những nhiệm vụ này có thể thực hiện được trong các script.

Hãy so sánh:

- `file_get_contents()`: Dễ dàng cho các nhiệm vụ đơn giản nhưng thiếu các tính năng nâng cao. Tốt cho việc nắm bắt nhanh chóng mà không rườm rà.
- `cURL`: Dao đa năng cho các yêu cầu web trong PHP. Xử lý các tình huống phức tạp như xác thực, cookie và thiết lập tiêu đề. Hơi nặng nề hơn, nhưng có sẵn khi bạn cần sức mạnh phụ trợ.

Đằng sau hậu trường, `file_get_contents()` gửi một yêu cầu GET tiêu chuẩn. Điều đó có nghĩa là nó hành động giống như một trình duyệt khi bạn nhập một URL. Nhưng không có ngữ cảnh HTTP (như các tiêu đề), một số trang có thể không trả về nội dung đúng.

`cURL`, ngược lại, có thể mô phỏng hành vi của trình duyệt một cách chính xác. Điều đó là cần thiết cho các trang web khó chịu mong đợi một số tiêu đề hoặc cookie nhất định.

Nhớ rằng, một số trang web không thích bị khai thác dữ liệu. Luôn tôn trọng `robots.txt` và các điều kiện dịch vụ.

## Xem thêm
- [Hướng dẫn PHP về file_get_contents()](http://php.net/manual/en/function.file-get-contents.php)
- [Hướng dẫn PHP về cURL](http://php.net/manual/en/book.curl.php)
- [Thông số kỹ thuật robots.txt](https://developers.google.com/search/docs/advanced/robots/robots_txt)
