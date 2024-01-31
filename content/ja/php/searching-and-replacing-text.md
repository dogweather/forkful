---
title:                "テキストの検索と置換"
date:                  2024-01-20T17:58:17.992038-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストの検索と置換"

category:             "PHP"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
テキストの検索と置換とは、指定した文字列を見つけて、それを別の文字列に変更するプロセスです。プログラマーは、コードのリファクタリング、データ修正、または自動化のためにこれを行います。

## How to:
以下のコード例を参照してください:

```php
<?php
// 元のテキスト
$text = 'こんにちは、世界！';

// str_replaceを使用してテキストを置換
$replacedText = str_replace('世界', 'プログラミングの世界', $text);

echo $replacedText; // 出力: こんにちは、プログラミングの世界！
?>
```

## Deep Dive
テキストの検索と置換は古くからプログラミングに役立っています。PHPでは`str_replace`関数がこの目的でよく使われます。正規表現が必要な複雑なパターンには`preg_replace`関数が利用されます。 `str_replace`は単純な文字列の置換に使うべきで、大規模なテキストデータには`strtr`関数が効率的かもしれません。

## See Also
- PHP Manual on `str_replace`: https://www.php.net/manual/en/function.str-replace.php
- PHP Manual on `preg_replace`: https://www.php.net/manual/en/function.preg-replace.php
- PHP Manual on `strtr`: https://www.php.net/manual/en/function.strtr.php
