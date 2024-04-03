---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:43.777140-07:00
description: "C\xE1ch th\u1EE9c: \u0110\u1ECDc XML v\u1EDBi SimpleXML."
lastmod: '2024-03-13T22:44:36.799501-06:00'
model: gpt-4-0125-preview
summary: "\u0110\u1ECDc XML v\u1EDBi SimpleXML."
title: "L\xE0m vi\u1EC7c v\u1EDBi XML"
weight: 40
---

## Cách thức:
Đọc XML với SimpleXML:

```php
$xmlString = '<?xml version="1.0" encoding="UTF-8"?>
              <note>
                <to>Tove</to>
                <from>Jani</from>
                <heading>Nhắc nhở</heading>
                <body>Đừng quên điều này</body>
              </note>';
              
$xml = simplexml_load_string($xmlString);

echo $xml->to;       // Kết quả: Tove
echo $xml->from;     // Kết quả: Jani
echo $xml->heading;  // Kết quả: Nhắc nhở
echo $xml->body;     // Kết quả: Đừng quên điều này
```

Viết XML với DOMDocument:

```php
$dom = new DOMDocument('1.0', 'UTF-8');

$root = $dom->createElement('note');
$dom->appendChild($root);

$to = $dom->createElement('to', 'Tove');
$from = $dom->createElement('from', 'Jani');
$heading = $dom->createElement('heading', 'Nhắc nhở');
$body = $dom->createElement('body', 'Đừng quên điều này');

$root->appendChild($to);
$root->appendChild($from);
$root->appendChild($heading);
$root->appendChild($body);

echo $dom->saveXML();
```

Ví dụ kết quả:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Nhắc nhở</heading>
  <body>Đừng quên điều này</body>
</note>
```

## Sâu hơn
XML, hay Ngôn ngữ Đánh dấu Mở rộng, đã trở thành một phần quan trọng trong việc tuần tự hóa dữ liệu kể từ khi được W3C đề xuất vào năm 1998. Nó rõ ràng, dễ đọc và nghiêm ngặt về cú pháp, làm cho nó trở thành sự lựa chọn đáng tin cậy cho tệp cấu hình, trao đổi dữ liệu và hơn thế nữa. Tuy nhiên, nó đã một phần bị làm lu mờ bởi JSON cho các API web do tính đơn giản và nhẹ của nó.

Lập trình viên thường chọn XML khi họ cần xác thực tài liệu được cung cấp bởi Sơ đồ XML hoặc khi làm việc trong các hệ sinh thái đã tự nó phụ thuộc nặng nề vào nó (như định dạng tệp của Microsoft Office). Xử lý XML trong PHP là đơn giản với phần mở rộng SimpleXML cho các thao tác cơ bản. Đối với việc điều khiển phức tạp hơn, DOMDocument cung cấp một bộ tính năng mạnh mẽ cho phép kiểm soát tốt hơn, chẳng hạn như xử lý không gian tên và xác thực sơ đồ.

## Xem thêm
- [PHP: SimpleXML](https://www.php.net/manual/en/book.simplexml.php)
- [PHP: DOMDocument](https://www.php.net/manual/en/class.domdocument.php)
- [W3Schools: Các phân tích XML trong PHP](https://www.w3schools.com/php/php_xml_parsers.asp)
- [W3C Sơ đồ XML](https://www.w3.org/XML/Schema)
