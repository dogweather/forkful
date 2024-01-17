---
title:                "文字列の内挿"
html_title:           "PHP: 文字列の内挿"
simple_title:         "文字列の内挿"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 何なんだっけ? (What & Why?)

PHPは、文字列を挿入することができるようにスクリプト言語を許可する。これを「インターポレーション」と呼び、一般的に変数を文字列に挿入するために使用される。プログラマーは、コードをより効率的にかつ読みやすくするために、インターポレーションを使用する。

## どうやるの? (How to:)

PHPでは、文字列をダブルクオートで囲むことで、変数を文字列に挿入することができる。例えば、"Hello $name!"のように、変数$nameを文字列に挿入することができる。また、変数を中括弧{}で囲んでも同じ結果になる。以下にコードと出力例を示す。

```PHP
$name = "John";
echo "Hello $name!";
// 出力: Hello John!

$name = "Jane";
echo "Hello {$name}!";
// 出力: Hello Jane!
```

## もっと詳しく (Deep Dive:)

インターポレーションは、PHPの初期バージョンで使われた機能であり、プログラマーにとって重要なツールである。変数を文字列に挿入する他の方法として、文字列結合を使用する方法がある。しかし、長い文字列や多数の変数がある場合、インターポレーションの方がコードがより読みやすく、効率的になる。また、変数を中括弧{}で囲むことで、複雑な式を文字列に挿入することもできる。

## 詳しくは (See Also:)

- [PHP公式ドキュメント - インターポレーション](https://www.php.net/manual/ja/language.types.string.php#language.types.string.parsing) 
- [PHPの歴史](https://www.php.net/history.php) 
- [文字列結合とインターポレーションの違い](https://www.w3schools.com/php/php_string.asp)