---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:04:09.149022-07:00
description: "Ph\xE2n t\xEDch c\xFA ph\xE1p HTML c\xF3 ngh\u0129a l\xE0 x\u1EED l\xFD\
  \ v\xE0 ph\xE2n t\xEDch c\u1EA5u tr\xFAc v\xE0 n\u1ED9i dung c\u1EE7a m\xE3 HTML.\
  \ L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 l\u1EA5y d\u1EEF\
  \ li\u1EC7u t\u1EEB trang web, thao t\xE1c\u2026"
lastmod: '2024-03-13T22:44:36.763431-06:00'
model: gpt-4-0125-preview
summary: "Ph\xE2n t\xEDch c\xFA ph\xE1p HTML c\xF3 ngh\u0129a l\xE0 x\u1EED l\xFD\
  \ v\xE0 ph\xE2n t\xEDch c\u1EA5u tr\xFAc v\xE0 n\u1ED9i dung c\u1EE7a m\xE3 HTML.\
  \ L\u1EADp tr\xECnh vi\xEAn l\xE0m \u0111i\u1EC1u n\xE0y \u0111\u1EC3 l\u1EA5y d\u1EEF\
  \ li\u1EC7u t\u1EEB trang web, thao t\xE1c\u2026"
title: "Ph\xE2n T\xEDch C\xFA Ph\xE1p HTML"
---

{{< edit_this_page >}}

## Gì & Tại Sao?
Phân tích cú pháp HTML có nghĩa là xử lý và phân tích cấu trúc và nội dung của mã HTML. Lập trình viên làm điều này để lấy dữ liệu từ trang web, thao tác hoặc trích xuất dữ liệu, tự động hóa việc kiểm thử, hoặc tích hợp nội dung web vào các ứng dụng.

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
