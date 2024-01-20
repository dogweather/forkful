---
title:                "文字列の先頭を大文字にする"
html_title:           "C: 文字列の先頭を大文字にする"
simple_title:         "文字列の先頭を大文字にする"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜか？)
文字列の大文字化は、文字列内のすべての文字を大文字に変換することです。これは、タイトルを整形したり、定数値を整理したりする際にプログラマーが行います。

## How to: (やり方)
```PHP
<?php
// 文字列を大文字に変換
$string = "こんにちは、世界！";
$capitalizedString = strtoupper($string);

echo $capitalizedString; // 出力: "こんにちは、世界！" (大文字には影響されない)
?>
```
PHPでは、`strtoupper` 関数を使って文字列を大文字にできますが、日本語などの多バイト文字はこの関数の範囲外です。

## Deep Dive (掘り下げ)
大文字化は初期のコンピュータシステムでよく使われました。これは、システムがテキストを一貫してかつシンプルに処理するためでした。`strtoupper` の他にも、`mb_strtoupper` 関数があり、これは多バイト文字（例えば日本語）の大文字化をサポートしています。なお、実装の詳細については、PHPの内部ではC言語が使用され、ロケール設定に依存することがあります。

## See Also (関連情報)
- PHPの公式ドキュメント [`strtoupper`](https://www.php.net/manual/en/function.strtoupper.php)
- PHPの公式ドキュメント [`mb_strtoupper`](https://www.php.net/manual/en/function.mb-strtoupper.php)
- Unicode文字の大文字化についての詳細は [Unicode Standard](https://unicode.org)