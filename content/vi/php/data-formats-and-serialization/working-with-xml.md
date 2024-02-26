---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:11:43.777140-07:00
description: "XML l\xE0 m\u1ED9t ng\xF4n ng\u1EEF \u0111\xE1nh d\u1EA5u \u0111\u01B0\
  \u1EE3c s\u1EED d\u1EE5ng \u0111\u1EC3 l\u01B0u tr\u1EEF v\xE0 v\u1EADn chuy\u1EC3\
  n d\u1EEF li\u1EC7u. L\u1EADp tr\xECnh vi\xEAn l\xE0m vi\u1EC7c v\u1EDBi XML \u0111\
  \u1EC3 cho ph\xE9p t\xEDnh t\u01B0\u01A1ng th\xEDch gi\u1EEFa c\xE1c \u1EE9ng d\u1EE5\
  ng\u2026"
lastmod: '2024-02-25T18:49:35.142774-07:00'
model: gpt-4-0125-preview
summary: "XML l\xE0 m\u1ED9t ng\xF4n ng\u1EEF \u0111\xE1nh d\u1EA5u \u0111\u01B0\u1EE3\
  c s\u1EED d\u1EE5ng \u0111\u1EC3 l\u01B0u tr\u1EEF v\xE0 v\u1EADn chuy\u1EC3n d\u1EEF\
  \ li\u1EC7u. L\u1EADp tr\xECnh vi\xEAn l\xE0m vi\u1EC7c v\u1EDBi XML \u0111\u1EC3\
  \ cho ph\xE9p t\xEDnh t\u01B0\u01A1ng th\xEDch gi\u1EEFa c\xE1c \u1EE9ng d\u1EE5\
  ng\u2026"
title: "L\xE0m vi\u1EC7c v\u1EDBi XML"
---

{{< edit_this_page >}}

## Cái gì & Tại sao?
XML là một ngôn ngữ đánh dấu được sử dụng để lưu trữ và vận chuyển dữ liệu. Lập trình viên làm việc với XML để cho phép tính tương thích giữa các ứng dụng và hệ thống - nghĩ về trao đổi dữ liệu và cài đặt cấu hình.

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
