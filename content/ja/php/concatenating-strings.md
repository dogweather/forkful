---
title:                "PHP: 「文字列の連結」"
simple_title:         "「文字列の連結」"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ文字列を連結するのか

文字列を連結することは、プログラミングにおいて非常に一般的なタスクです。例えば、文章やメールなどのテンプレートを作成する場合や、複数のデータを組み合わせて処理する場合に使われます。文字列を連結することで、より柔軟にデータを扱うことができます。

## 連結の方法

PHPで文字列を連結するには、2つの方法があります。1つは `.`（ドット）演算子を使う方法、もう1つは `concat()` 関数を使う方法です。以下に例を示します。

```PHP
<?php

// "."演算子を使う方法
$name = '山田';
$greeting = 'こんにちは';
$message = $greeting . $name;
echo $message; // 出力結果: こんにちは山田

// concat() 関数を使う方法
$name = '山田';
$greeting = 'こんにちは';
$message = concat($greeting, $name);
echo $message; // 出力結果: こんにちは山田
```

上記の例では、`$greeting` と `$name` を連結して `こんにちは山田` というメッセージを作成しています。

## 深堀り

文字列の連結は、複数のデータを効率的に組み合わせることができるため、非常に重要な機能です。また、他のプログラミング言語でも似たような方法で文字列を連結することができるため、この方法を覚えておくと他の言語への移行もスムーズに行うことができます。さらに、文字列の連結はパフォーマンスにも影響を与えるため、最適な方法を選択することも重要です。

## See Also

- [PHPのドキュメント - 文字列](https://www.php.net/manual/ja/language.types.string.php)
- [PHPで文字列を扱う方法](https://programming-pocket.blog.ss-blog.jp/2021-04-15)
- [PHP独自の機能であるヒアドキュメント](https://qiita.com/rtrava3/items/ac50795c5be57fbd5c26)