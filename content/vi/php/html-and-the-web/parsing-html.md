---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:09.149022-07:00
description: "C\xE1ch th\u1EF1c hi\u1EC7n: PHP c\xF3 m\u1ED9t s\u1ED1 th\u01B0 vi\u1EC7\
  n c\xF3 s\u1EB5n \u0111\u1EC3 ph\xE2n t\xEDch c\xFA ph\xE1p HTML, nh\u01B0 DOMDocument.\
  \ D\u01B0\u1EDBi \u0111\xE2y l\xE0 m\u1ED9t v\xED d\u1EE5 s\u1EED d\u1EE5ng \u0111\
  \u01A1n gi\u1EA3n."
lastmod: '2024-03-13T22:44:36.763431-06:00'
model: gpt-4-0125-preview
summary: "PHP c\xF3 m\u1ED9t s\u1ED1 th\u01B0 vi\u1EC7n c\xF3 s\u1EB5n \u0111\u1EC3\
  \ ph\xE2n t\xEDch c\xFA ph\xE1p HTML, nh\u01B0 DOMDocument."
title: "Ph\xE2n T\xEDch C\xFA Ph\xE1p HTML"
weight: 43
---

## Cách thực hiện:
PHP có một số thư viện có sẵn để phân tích cú pháp HTML, như DOMDocument. Dưới đây là một ví dụ sử dụng đơn giản:

```php
<?php
$htmlString = '<!DOCTYPE html><html><body><h1>Xin chào, Thế giới!</h1></body></html>';
$dom = new DOMDocument();
@$dom->loadHTML($htmlString); // '@' ức chế cảnh báo được gây ra bởi cấu trúc HTML không hợp lệ
$h1Tags = $dom->getElementsByTagName('h1');

foreach ($h1Tags as $tag) {
    echo $tag->nodeValue; // Đầu ra: Xin chào, Thế giới!
}
?>
```

Kịch bản này xuất ra: `Xin chào, Thế giới!`

## Sâu hơn nữa
Trở lại những ngày đầu của web, chúng ta lấy HTML bằng regex và các giải pháp tự phát, nhưng nó rất l messy. Enter `DOMDocument` và `SimpleXMLElement`, với việc phân tích cú pháp HTML và XML đúng đắn kể từ PHP 5. Chúng cho phép bạn điều hướng và thao tác với HTML như một cấu trúc dạng cây.

Ngày nay, trong khi `DOMDocument` là lựa chọn của bạn để xử lý phân tích cú pháp nội bộ, các lựa chọn khác như `SimpleHTMLDom` và `phpQuery` cung cấp thêm đường cú pháp và có thể thân thiện hơn đối với những người đến từ nền tảng JavaScript/jQuery.

Nội bộ, `DOMDocument` chuyển đổi HTML thành một cây DOM, giúp dễ dàng truy cập các phần tử cụ thể, thay đổi thuộc tính, và thậm chí sửa đổi tài liệu ngay lập tức. Một điều thú vị về `DOMDocument` là khả năng dung nạp của nó đối với HTML kém, làm sạch nó và cho phép bạn làm việc với các trang web thực tế không luôn được định dạng hoàn hảo.

## Xem Thêm
- [DOMDocument trên PHP.net](https://www.php.net/manual/en/class.domdocument.php)
- [SimpleXML cho việc xử lý các nhiệm vụ XML cơ bản](https://www.php.net/manual/en/book.simplexml.php)
- [dự án SimpleHTMLDom trên SourceForge](https://sourceforge.net/projects/simplehtmldom/)
- [kho lưu trữ phpQuery trên GitHub](https://github.com/punkave/phpQuery)
