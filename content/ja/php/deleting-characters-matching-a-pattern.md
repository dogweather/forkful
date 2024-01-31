---
title:                "パターンに一致する文字を削除する"
date:                  2024-01-20T17:42:43.869063-07:00
model:                 gpt-4-1106-preview
simple_title:         "パターンに一致する文字を削除する"

category:             "PHP"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
データを扱う際、特定のパターンにマッチする文字を削除する必要がよくあります。これは、無用な記号やスペースを取り除き、データの整形や分析を容易にするためです。

## How to: (方法)
PHPで文字列から特定のパターンを削除するには、`preg_replace` 関数が便利です。以下の例を見てみましょう。

```php
<?php
$inputString = "Hello, 123 World! This is a 456 test.";
// \d は数字に匹配するパターンです。
$pattern = '/\d/';

// 空文字列を置き換えることで、数字を削除しています。
$outputString = preg_replace($pattern, '', $inputString);

echo $outputString; // "Hello,  World! This is a  test."
?>
```

`preg_replace` はマッチしたすべての部分を置き換えます。パターンをきちんと指定して使いましょう。

## Deep Dive (深掘り)
文字の削除は、イテレーティブな文字処理の最も基本的なタイプの一つです。初期の文字列操作関数の中でも、`str_replace`や`ereg_replace`（後に非推奨とされました）などがありました。 

`preg_replace` は、より強力な "Perl互換正規表現" (PCRE) をサポートしています。PCREは1980年代後半にPerlで導入されたもので、その強力なパターンマッチングと操作能力により、多くの言語が取り入れています。

代替手段として、シンプルな文字列削除には `str_replace` や `str_ireplace`（大文字小文字を無視する）が使えますが、パターンマッチングには適していません。

実装の観点からは、`preg_replace` 関数は内部でコンパイル済みの正規表現をキャッシュし、パフォーマンスを高めています。しかし、正規表現は複雑になる傾向があるため、シンプルな置換には適さないこともあります。バランスを見極めましょう。

## See Also (参照)
- [PHP: preg_replace - Manual](https://www.php.net/manual/en/function.preg-replace.php)
- [PHP: Regular Expressions (Perl-Compatible) - Manual](https://www.php.net/manual/en/book.pcre.php)
- [PHP: str_replace - Manual](https://www.php.net/manual/en/function.str-replace.php)

これらのリンクから、さらなる例や詳細なドキュメントを見ることができます。パターンマッチングと文字列処理について、しっかりと学びましょう。
