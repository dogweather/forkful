---
date: 2024-01-26 04:34:24.936233-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u4F7F\u7528SimpleXML\u8BFB\u53D6XML\uFF1A\
  ."
lastmod: '2024-04-05T22:38:47.048789-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u4F7F\u7528SimpleXML\u8BFB\u53D6XML\uFF1A\
  ."
title: "\u5904\u7406XML"
weight: 40
---

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
