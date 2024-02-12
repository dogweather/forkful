---
title:                "Phân Tích Cú Pháp HTML"
aliases: - /vi/php/parsing-html.md
date:                  2024-01-28T22:04:09.149022-07:00
model:                 gpt-4-0125-preview
simple_title:         "Phân Tích Cú Pháp HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
