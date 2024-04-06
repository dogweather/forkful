---
date: 2024-01-20 17:42:43.869063-07:00
description: "How to: (\u65B9\u6CD5) PHP\u3067\u6587\u5B57\u5217\u304B\u3089\u7279\
  \u5B9A\u306E\u30D1\u30BF\u30FC\u30F3\u3092\u524A\u9664\u3059\u308B\u306B\u306F\u3001\
  `preg_replace` \u95A2\u6570\u304C\u4FBF\u5229\u3067\u3059\u3002\u4EE5\u4E0B\u306E\
  \u4F8B\u3092\u898B\u3066\u307F\u307E\u3057\u3087\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.082568-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) PHP\u3067\u6587\u5B57\u5217\u304B\u3089\u7279\u5B9A\u306E\
  \u30D1\u30BF\u30FC\u30F3\u3092\u524A\u9664\u3059\u308B\u306B\u306F\u3001`preg_replace`\
  \ \u95A2\u6570\u304C\u4FBF\u5229\u3067\u3059\u3002\u4EE5\u4E0B\u306E\u4F8B\u3092\
  \u898B\u3066\u307F\u307E\u3057\u3087\u3046\u3002"
title: "\u30D1\u30BF\u30FC\u30F3\u306B\u4E00\u81F4\u3059\u308B\u6587\u5B57\u3092\u524A\
  \u9664\u3059\u308B"
weight: 5
---

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
