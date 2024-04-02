---
date: 2024-01-20 17:35:10.003664-07:00
description: "\u6587\u5B57\u5217\u9023\u7D50\u306F\u3001\uFF12\u3064\u4EE5\u4E0A\u306E\
  \u6587\u5B57\u5217\u3092\u7D50\u5408\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30C7\
  \u30FC\u30BF\u306E\u8868\u793A\u3001SQL\u30AF\u30A8\u30EA\u306E\u4F5C\u6210\u3001\
  \u30ED\u30B0\u30E1\u30C3\u30BB\u30FC\u30B8\u306E\u751F\u6210\u306A\u3069\u3001\u69D8\
  \u3005\u306A\u6587\u8108\u3067\u5FC5\u8981\u3068\u3055\u308C\u3066\u3044\u307E\u3059\
  \u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.233719-06:00'
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u9023\u7D50\u306F\u3001\uFF12\u3064\u4EE5\u4E0A\u306E\
  \u6587\u5B57\u5217\u3092\u7D50\u5408\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30C7\
  \u30FC\u30BF\u306E\u8868\u793A\u3001SQL\u30AF\u30A8\u30EA\u306E\u4F5C\u6210\u3001\
  \u30ED\u30B0\u30E1\u30C3\u30BB\u30FC\u30B8\u306E\u751F\u6210\u306A\u3069\u3001\u69D8\
  \u3005\u306A\u6587\u8108\u3067\u5FC5\u8981\u3068\u3055\u308C\u3066\u3044\u307E\u3059\
  \u3002"
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
weight: 3
---

## What & Why? (何となぜ?)
文字列連結は、２つ以上の文字列を結合することです。データの表示、SQLクエリの作成、ログメッセージの生成など、様々な文脈で必要とされています。

## How to (方法)
PHPではドット(`.`)を使って文字列を連結します。簡単で直感的な方法です。下記例を見てください。

```php
<?php
$greeting = "こんにちは";
$name = "世界";
$combinedMessage = $greeting . ", " . $name . "!";
echo $combinedMessage;
```

出力:
```
こんにちは, 世界!
```

変数間にスペースが必要なら、文字列の間に追加してください。

## Deep Dive (掘り下げ)
以前のPHPバージョンでは、文字列の連結は今とほぼ同じでした。しかし、性能はバージョンアップごとに向上しています。

代替方法としては、配列を使って`implode()`関数で文字列を結合することもできます。たとえば:

```php
<?php
$parts = ["こんにちは", "世界"];
$combinedMessage = implode(", ", $parts) . "!";
echo $combinedMessage;
```

実装の詳細では、PHPの内部では、連結された新しい文字列はメモリ上で新しい場所に保存されます。それぞれの連結でメモリ使用量が増加する可能性があるため、非常に大きな文字列を扱う場合は注意が必要です。

## See Also (関連項目)
- PHP公式ドキュメントの文字列処理について: https://www.php.net/manual/ja/book.strings.php
- 文字列操作のパフォーマンス比較: https://phpbench.com/
