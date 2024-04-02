---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:02.291102-07:00
description: "T\u1EA3i xu\u1ED1ng m\u1ED9t trang web c\xF3 ngh\u0129a l\xE0 n\u1EAF\
  m b\u1EAFt n\u1ED9i dung tr\u1EF1c tuy\u1EBFn \u0111\u1EC3 b\u1EA1n c\xF3 th\u1EC3\
  \ s\u1EED d\u1EE5ng ho\u1EB7c ph\xE2n t\xEDch n\xF3 ngo\u1EA1i tuy\u1EBFn. L\u1EAD\
  p tr\xECnh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3\u2026"
lastmod: '2024-03-13T22:44:36.764713-06:00'
model: gpt-4-0125-preview
summary: "T\u1EA3i xu\u1ED1ng m\u1ED9t trang web c\xF3 ngh\u0129a l\xE0 n\u1EAFm b\u1EAF\
  t n\u1ED9i dung tr\u1EF1c tuy\u1EBFn \u0111\u1EC3 b\u1EA1n c\xF3 th\u1EC3 s\u1EED\
  \ d\u1EE5ng ho\u1EB7c ph\xE2n t\xEDch n\xF3 ngo\u1EA1i tuy\u1EBFn. L\u1EADp tr\xEC\
  nh vi\xEAn th\u1EF1c hi\u1EC7n vi\u1EC7c n\xE0y \u0111\u1EC3\u2026"
title: "T\u1EA3i trang web"
weight: 42
---

## Cái gì & Tại sao?

Tải xuống một trang web có nghĩa là nắm bắt nội dung trực tuyến để bạn có thể sử dụng hoặc phân tích nó ngoại tuyến. Lập trình viên thực hiện việc này để lấy dữ liệu trên web, phân tích dữ liệu hoặc tương tác với nội dung web theo cách lập trình.

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
