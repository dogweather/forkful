---
title:                "XMLの扱い方"
aliases:
- ja/php/working-with-xml.md
date:                  2024-01-26T04:34:14.688342-07:00
model:                 gpt-4-0125-preview
simple_title:         "XMLの扱い方"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/working-with-xml.md"
---

{{< edit_this_page >}}

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
