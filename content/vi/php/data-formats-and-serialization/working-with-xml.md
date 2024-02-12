---
title:                "Làm việc với XML"
date:                  2024-01-28T22:11:43.777140-07:00
model:                 gpt-4-0125-preview
simple_title:         "Làm việc với XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/vi/php/working-with-xml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
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
