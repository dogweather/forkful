---
date: 2024-01-26 04:34:14.688342-07:00
description: ''
lastmod: '2024-04-05T22:50:56.191928-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A XML\uFF08eXtensible Markup Language\uFF09\u306F\u3001\
  1998\u5E74\u306BW3C\u306E\u63A8\u5968\u4E8B\u9805\u3068\u3057\u3066\u4EE5\u6765\u3001\
  \u30C7\u30FC\u30BF\u306E\u30B7\u30EA\u30A2\u30E9\u30A4\u30BC\u30FC\u30B7\u30E7\u30F3\
  \u306B\u304A\u3044\u3066\u91CD\u8981\u306A\u5F79\u5272\u3092\u679C\u305F\u3057\u3066\
  \u304D\u307E\u3057\u305F\u3002\u305D\u308C\u306F\u5197\u9577\u3067\u3001\u4EBA\u9593\
  \u304C\u8AAD\u3080\u3053\u3068\u304C\u3067\u304D\u3001\u69CB\u6587\u306B\u53B3\u3057\
  \u3044\u305F\u3081\u3001\u69CB\u6210\u30D5\u30A1\u30A4\u30EB\u3001\u30C7\u30FC\u30BF\
  \u306E\u4EA4\u63DB\u3001\u305D\u306E\u4ED6\u591A\u304F\u306E\u7528\u9014\u306B\u4FE1\
  \u983C\u3067\u304D\u308B\u9078\u629E\u80A2\u3068\u306A\u3063\u3066\u3044\u307E\u3059\
  \u3002\u305F\u3060\u3057\u3001\u305D\u306E\u5358\u7D14\u3055\u3084\u8EFD\u91CF\u6027\
  \u306E\u305F\u3081\u306B\u3001JSON\u306B\u3088\u3063\u3066Web API\u3067\u90E8\u5206\
  \u7684\u306B\u5F71\u3092\u6F5C\u3081\u3066\u3044\u307E\u3059\u3002 XML\u30B9\u30AD\
  \u30FC\u30DE\u306B\u3088\u308B\u30C9\u30AD\u30E5\u30E1\u30F3\u30C8\u306E\u691C\u8A3C\
  \u304C\u5FC5\u8981\u306A\u5834\u5408\u3084\u3001\u65E2\u306B\u305D\u308C\u306B\u5927\
  \u304D\u304F\u4F9D\u5B58\u3057\u3066\u3044\u308B\u30A8\u30B3\u30B7\u30B9\u30C6\u30E0\
  \uFF08Microsoft Office\u30D5\u30A1\u30A4\u30EB\u5F62\u5F0F\u306A\u3069\uFF09\u5185\
  \u3067\u4F5C\u696D\u3059\u308B\u5834\u5408\u306A\u3069\u3001\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3057\u3070\u3057\u3070XML\u3092\u9078\u629E\u3057\u307E\u3059\
  \u3002PHP\u3067\u306EXML\u306E\u6271\u3044\u306F\u3001\u57FA\u672C\u64CD\u4F5C\u306B\
  \u306FSimpleXML\u62E1\u5F35\u6A5F\u80FD\u3092\u4F7F\u3063\u3066\u7C21\u5358\u3067\
  \u3059\u3002\u3088\u308A\u8907\u96D1\u306A\u64CD\u4F5C\u306B\u5BFE\u3057\u3066\u306F\
  \u3001\u540D\u524D\u7A7A\u9593\u306E\u6271\u3044\u3084\u30B9\u30AD\u30FC\u30DE\u691C\
  \u8A3C\u306A\u3069\u3001\u3088\u308A\u5927\u304D\u306A\u5236\u5FA1\u3092\u53EF\u80FD\
  \u306B\u3059\u308B\u6A5F\u80FD\u306E\u5805\u7262\u306A\u30BB\u30C3\u30C8\u3092\u63D0\
  \u4F9B\u3059\u308BDOMDocument\u304C\u3042\u308A\u307E\u3059\u3002"
title: "XML\u306E\u6271\u3044\u65B9"
weight: 40
---

## 方法：
SimpleXMLでのXML読み込み：

```php
$xmlString = '<?xml version="1.0" encoding="UTF-8"?>
              <note>
                <to>Tove</to>
                <from>Jani</from>
                <heading>Reminder</heading>
                <body>Don't forget this</body>
              </note>';
              
$xml = simplexml_load_string($xmlString);

echo $xml->to;       // 出力：Tove
echo $xml->from;     // 出力：Jani
echo $xml->heading;  // 出力：Reminder
echo $xml->body;     // 出力：Don't forget this
```

DOMDocumentでのXML書き込み：

```php
$dom = new DOMDocument('1.0', 'UTF-8');

$root = $dom->createElement('note');
$dom->appendChild($root);

$to = $dom->createElement('to', 'Tove');
$from = $dom->createElement('from', 'Jani');
$heading = $dom->createElement('heading', 'Reminder');
$body = $dom->createElement('body', 'Don't forget this');

$root->appendChild($to);
$root->appendChild($from);
$root->appendChild($heading);
$root->appendChild($body);

echo $dom->saveXML();
```

出力例：

```xml
<?xml version="1.0" encoding="UTF-8"?>
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Reminder</heading>
  <body>Don't forget this</body>
</note>
```

## 深堀り
XML（eXtensible Markup Language）は、1998年にW3Cの推奨事項として以来、データのシリアライゼーションにおいて重要な役割を果たしてきました。それは冗長で、人間が読むことができ、構文に厳しいため、構成ファイル、データの交換、その他多くの用途に信頼できる選択肢となっています。ただし、その単純さや軽量性のために、JSONによってWeb APIで部分的に影を潜めています。

XMLスキーマによるドキュメントの検証が必要な場合や、既にそれに大きく依存しているエコシステム（Microsoft Officeファイル形式など）内で作業する場合など、プログラマーはしばしばXMLを選択します。PHPでのXMLの扱いは、基本操作にはSimpleXML拡張機能を使って簡単です。より複雑な操作に対しては、名前空間の扱いやスキーマ検証など、より大きな制御を可能にする機能の堅牢なセットを提供するDOMDocumentがあります。

## 参照
- [PHP: SimpleXML](https://www.php.net/manual/ja/book.simplexml.php)
- [PHP: DOMDocument](https://www.php.net/manual/ja/class.domdocument.php)
- [W3Schools: PHP XML Parsers](https://www.w3schools.com/php/php_xml_parsers.asp)
- [W3C XML Schema](https://www.w3.org/XML/Schema)
