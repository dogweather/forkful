---
title:                "文字列の大文字化"
html_title:           "PHP: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why (なぜ)
文字列を大文字にすることの利点は、読みやすさやデータ整理に優れていることです。

## How To (方法)
```PHP
$string = "hello, world";
echo strtoupper($string); // HELLO, WORLD
```
このように `strtoupper` 関数を使うことで、文字列を大文字に変換することができます。

## Deep Dive (詳細)
大文字にすることで、複数の単語から成る文字列を一貫性のある形式に統一することができます。また、データの比較や検索を行う際にも、大文字に統一することで正しい結果を得ることができます。さらに、国際化したアプリケーションを開発する際にも、大文字にすることで言語の違いによる混乱を防ぐことができます。

## See Also (関連リンク)
- [PHP: strtoupper - Manual](https://www.php.net/manual/en/function.strtoupper.php)
- [Advantages of using upper case naming](https://stackoverflow.com/questions/6154363/advantages-of-using-upper-case-naming)
- [Principles of designing for global markets](http://zhenkewu.com/articles/globalization/2005/04/principles-of-designing-for-global-markets/)