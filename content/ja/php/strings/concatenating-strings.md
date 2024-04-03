---
date: 2024-01-20 17:35:10.003664-07:00
description: "How to (\u65B9\u6CD5) PHP\u3067\u306F\u30C9\u30C3\u30C8(`.`)\u3092\u4F7F\
  \u3063\u3066\u6587\u5B57\u5217\u3092\u9023\u7D50\u3057\u307E\u3059\u3002\u7C21\u5358\
  \u3067\u76F4\u611F\u7684\u306A\u65B9\u6CD5\u3067\u3059\u3002\u4E0B\u8A18\u4F8B\u3092\
  \u898B\u3066\u304F\u3060\u3055\u3044\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.233719-06:00'
model: gpt-4-1106-preview
summary: "PHP\u3067\u306F\u30C9\u30C3\u30C8(`.`)\u3092\u4F7F\u3063\u3066\u6587\u5B57\
  \u5217\u3092\u9023\u7D50\u3057\u307E\u3059\u3002\u7C21\u5358\u3067\u76F4\u611F\u7684\
  \u306A\u65B9\u6CD5\u3067\u3059\u3002\u4E0B\u8A18\u4F8B\u3092\u898B\u3066\u304F\u3060\
  \u3055\u3044."
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
weight: 3
---

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
