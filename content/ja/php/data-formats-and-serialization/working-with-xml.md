---
date: 2024-01-26 04:34:14.688342-07:00
description: "XML\u306F\u30C7\u30FC\u30BF\u306E\u4FDD\u5B58\u304A\u3088\u3073\u8F38\
  \u9001\u306E\u305F\u3081\u306B\u4F7F\u7528\u3055\u308C\u308B\u30DE\u30FC\u30AF\u30A2\
  \u30C3\u30D7\u8A00\u8A9E\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3084\u30B7\u30B9\u30C6\u30E0\
  \u9593\u306E\u76F8\u4E92\u904B\u7528\u6027\u3092\u5B9F\u73FE\u3059\u308B\u305F\u3081\
  \u306BXML\u3092\u4F7F\u7528\u3057\u307E\u3059 - \u30C7\u30FC\u30BF\u4EA4\u63DB\u3084\
  \u8A2D\u5B9A\u306E\u69CB\u6210\u3092\u8003\u3048\u3066\u304F\u3060\u3055\u3044\u3002"
lastmod: '2024-03-13T22:44:42.278640-06:00'
model: gpt-4-0125-preview
summary: "XML\u306F\u30C7\u30FC\u30BF\u306E\u4FDD\u5B58\u304A\u3088\u3073\u8F38\u9001\
  \u306E\u305F\u3081\u306B\u4F7F\u7528\u3055\u308C\u308B\u30DE\u30FC\u30AF\u30A2\u30C3\
  \u30D7\u8A00\u8A9E\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\
  \u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u3084\u30B7\u30B9\u30C6\u30E0\u9593\
  \u306E\u76F8\u4E92\u904B\u7528\u6027\u3092\u5B9F\u73FE\u3059\u308B\u305F\u3081\u306B\
  XML\u3092\u4F7F\u7528\u3057\u307E\u3059 - \u30C7\u30FC\u30BF\u4EA4\u63DB\u3084\u8A2D\
  \u5B9A\u306E\u69CB\u6210\u3092\u8003\u3048\u3066\u304F\u3060\u3055\u3044\u3002."
title: "XML\u306E\u6271\u3044\u65B9"
weight: 40
---

## 何となぜ？
XMLはデータの保存および輸送のために使用されるマークアップ言語です。プログラマーは、アプリケーションやシステム間の相互運用性を実現するためにXMLを使用します - データ交換や設定の構成を考えてください。

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
