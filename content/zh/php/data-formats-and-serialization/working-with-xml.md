---
title:                "处理XML"
aliases: - /zh/php/working-with-xml.md
date:                  2024-01-26T04:34:24.936233-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/working-with-xml.md"
---

{{< edit_this_page >}}

## 是什么 & 为什么？
XML是一种用于存储和传输数据的标记语言。程序员通过处理XML来实现应用程序和系统之间的互操作性 - 想象一下数据交换和配置设置。

## 如何操作：
使用SimpleXML读取XML：

```php
$xmlString = '<?xml version="1.0" encoding="UTF-8"?>
              <note>
                <to>Tove</to>
                <from>Jani</from>
                <heading>Reminder</heading>
                <body>别忘了这个</body>
              </note>';
              
$xml = simplexml_load_string($xmlString);

echo $xml->to;       // 输出：Tove
echo $xml->from;     // 输出：Jani
echo $xml->heading;  // 输出：Reminder
echo $xml->body;     // 输出：别忘了这个
```

使用DOMDocument写入XML：

```php
$dom = new DOMDocument('1.0', 'UTF-8');

$root = $dom->createElement('note');
$dom->appendChild($root);

$to = $dom->createElement('to', 'Tove');
$from = $dom->createElement('from', 'Jani');
$heading = $dom->createElement('heading', 'Reminder');
$body = $dom->createElement('body', '别忘了这个');

$root->appendChild($to);
$root->appendChild($from);
$root->appendChild($heading);
$root->appendChild($body);

echo $dom->saveXML();
```

示例输出：

```xml
<?xml version="1.0" encoding="UTF-8"?>
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Reminder</heading>
  <body>别忘了这个</body>
</note>
```

## 深入探讨
XML，或可扩展标记语言，自1998年被W3C推荐以来，一直是数据序列化的主要方式。它详细、易读且在语法上严格，使其成为配置文件、数据交换等的可靠选择。然而，由于其简洁和轻量级的特性，JSON在Web APIs中部分取代了它的位置。

当程序员需要XML架构提供的文档验证，或者在已经大量依赖XML的生态系统（如Microsoft Office文件格式）中工作时，他们通常会选择XML。在PHP中处理XML非常直接，SimpleXML扩展支持基本操作。对于更复杂的操作，DOMDocument提供了一套功能强大的特性，允许更大的控制，如命名空间处理和架构验证。

## 另请参阅
- [PHP：SimpleXML](https://www.php.net/manual/en/book.simplexml.php)
- [PHP：DOMDocument](https://www.php.net/manual/en/class.domdocument.php)
- [W3Schools：PHP XML解析器](https://www.w3schools.com/php/php_xml_parsers.asp)
- [W3C XML架构](https://www.w3.org/XML/Schema)
