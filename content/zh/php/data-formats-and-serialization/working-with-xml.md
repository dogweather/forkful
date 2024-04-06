---
date: 2024-01-26 04:34:24.936233-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A XML\uFF0C\u6216\u53EF\u6269\u5C55\u6807\
  \u8BB0\u8BED\u8A00\uFF0C\u81EA1998\u5E74\u88ABW3C\u63A8\u8350\u4EE5\u6765\uFF0C\u4E00\
  \u76F4\u662F\u6570\u636E\u5E8F\u5217\u5316\u7684\u4E3B\u8981\u65B9\u5F0F\u3002\u5B83\
  \u8BE6\u7EC6\u3001\u6613\u8BFB\u4E14\u5728\u8BED\u6CD5\u4E0A\u4E25\u683C\uFF0C\u4F7F\
  \u5176\u6210\u4E3A\u914D\u7F6E\u6587\u4EF6\u3001\u6570\u636E\u4EA4\u6362\u7B49\u7684\
  \u53EF\u9760\u9009\u62E9\u3002\u7136\u800C\uFF0C\u7531\u4E8E\u5176\u7B80\u6D01\u548C\
  \u8F7B\u91CF\u7EA7\u7684\u7279\u6027\uFF0CJSON\u5728Web APIs\u4E2D\u90E8\u5206\u53D6\
  \u4EE3\u4E86\u5B83\u7684\u4F4D\u7F6E\u3002\u2026"
lastmod: '2024-04-05T22:51:01.098629-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A XML\uFF0C\u6216\u53EF\u6269\u5C55\u6807\u8BB0\
  \u8BED\u8A00\uFF0C\u81EA1998\u5E74\u88ABW3C\u63A8\u8350\u4EE5\u6765\uFF0C\u4E00\u76F4\
  \u662F\u6570\u636E\u5E8F\u5217\u5316\u7684\u4E3B\u8981\u65B9\u5F0F\u3002\u5B83\u8BE6\
  \u7EC6\u3001\u6613\u8BFB\u4E14\u5728\u8BED\u6CD5\u4E0A\u4E25\u683C\uFF0C\u4F7F\u5176\
  \u6210\u4E3A\u914D\u7F6E\u6587\u4EF6\u3001\u6570\u636E\u4EA4\u6362\u7B49\u7684\u53EF\
  \u9760\u9009\u62E9\u3002\u7136\u800C\uFF0C\u7531\u4E8E\u5176\u7B80\u6D01\u548C\u8F7B\
  \u91CF\u7EA7\u7684\u7279\u6027\uFF0CJSON\u5728Web APIs\u4E2D\u90E8\u5206\u53D6\u4EE3\
  \u4E86\u5B83\u7684\u4F4D\u7F6E\u3002 \u5F53\u7A0B\u5E8F\u5458\u9700\u8981XML\u67B6\
  \u6784\u63D0\u4F9B\u7684\u6587\u6863\u9A8C\u8BC1\uFF0C\u6216\u8005\u5728\u5DF2\u7ECF\
  \u5927\u91CF\u4F9D\u8D56XML\u7684\u751F\u6001\u7CFB\u7EDF\uFF08\u5982Microsoft Office\u6587\
  \u4EF6\u683C\u5F0F\uFF09\u4E2D\u5DE5\u4F5C\u65F6\uFF0C\u4ED6\u4EEC\u901A\u5E38\u4F1A\
  \u9009\u62E9XML\u3002\u5728PHP\u4E2D\u5904\u7406XML\u975E\u5E38\u76F4\u63A5\uFF0C\
  SimpleXML\u6269\u5C55\u652F\u6301\u57FA\u672C\u64CD\u4F5C\u3002\u5BF9\u4E8E\u66F4\
  \u590D\u6742\u7684\u64CD\u4F5C\uFF0CDOMDocument\u63D0\u4F9B\u4E86\u4E00\u5957\u529F\
  \u80FD\u5F3A\u5927\u7684\u7279\u6027\uFF0C\u5141\u8BB8\u66F4\u5927\u7684\u63A7\u5236\
  \uFF0C\u5982\u547D\u540D\u7A7A\u95F4\u5904\u7406\u548C\u67B6\u6784\u9A8C\u8BC1\u3002"
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
