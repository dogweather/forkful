---
date: 2024-01-26 04:34:24.936233-07:00
description: "XML\u662F\u4E00\u79CD\u7528\u4E8E\u5B58\u50A8\u548C\u4F20\u8F93\u6570\
  \u636E\u7684\u6807\u8BB0\u8BED\u8A00\u3002\u7A0B\u5E8F\u5458\u901A\u8FC7\u5904\u7406\
  XML\u6765\u5B9E\u73B0\u5E94\u7528\u7A0B\u5E8F\u548C\u7CFB\u7EDF\u4E4B\u95F4\u7684\
  \u4E92\u64CD\u4F5C\u6027 - \u60F3\u8C61\u4E00\u4E0B\u6570\u636E\u4EA4\u6362\u548C\
  \u914D\u7F6E\u8BBE\u7F6E\u3002"
lastmod: '2024-03-11T00:14:21.683636-06:00'
model: gpt-4-0125-preview
summary: "XML\u662F\u4E00\u79CD\u7528\u4E8E\u5B58\u50A8\u548C\u4F20\u8F93\u6570\u636E\
  \u7684\u6807\u8BB0\u8BED\u8A00\u3002\u7A0B\u5E8F\u5458\u901A\u8FC7\u5904\u7406XML\u6765\
  \u5B9E\u73B0\u5E94\u7528\u7A0B\u5E8F\u548C\u7CFB\u7EDF\u4E4B\u95F4\u7684\u4E92\u64CD\
  \u4F5C\u6027 - \u60F3\u8C61\u4E00\u4E0B\u6570\u636E\u4EA4\u6362\u548C\u914D\u7F6E\
  \u8BBE\u7F6E\u3002"
title: "\u5904\u7406XML"
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
