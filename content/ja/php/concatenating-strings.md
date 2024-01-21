---
title:                "文字列の連結"
date:                  2024-01-20T17:35:10.003664-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の連結"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/concatenating-strings.md"
---

{{< edit_this_page >}}

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