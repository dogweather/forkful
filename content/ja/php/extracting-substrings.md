---
title:                "PHP: 文字列の抽出"
simple_title:         "文字列の抽出"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

サブストリングの抽出に取り組む理由は何でしょうか？サブストリングとは、文字列の一部を抽出することを指します。これは、データ処理やテキスト分析などの多くのプログラミングタスクで必要とされる基本的な操作です。PHPを使ってサブストリングを抽出する方法を学ぶことで、より多くの作業を効率的かつ正確に行うことができます。

## 方法

サブストリングを抽出するためには、PHPの組み込み関数である`substr()`を使用します。この関数には2つの必須のパラメーターがあります。第一引数は抽出したい文字列であり、第二引数は開始位置を指定します。例えば、次のようにコードを記述することで、文字列の先頭から3文字目からのサブストリングを抽出することができます。

```PHP
<?php
    $string = "これはサンプルの文章です";
    $sub = substr($string, 3);
    echo $sub;
```

上記のコードの出力は`はサンプルの文章です`となります。また、第三引数を指定することで、抽出する文字数を制限することもできます。例えば、`substr($string, 3, 5)`とすることで、抽出される文字数は5文字までとなり、出力は`はサンプ`となります。詳細な使い方はPHPの公式ドキュメントを参照してください。

## ディープダイブ

サブストリングの抽出にはさまざまなオプションがあり、これらを組み合わせることでより複雑な処理を行うことができます。例えば、マルチバイト文字を含む文字列を抽出する際、`mb_substr()`関数を使用することで文字化けを防ぐことができます。また、正規表現を使用して特定のパターンにマッチする文字列を抽出することも可能です。サブストリングの抽出についてさらに詳しく学ぶことで、より柔軟な処理を行うことができるようになります。

## See Also

- [PHP: substr - Manual](https://www.php.net/manual/en/function.substr.php)
- [PHP: mb_substr - Manual](https://www.php.net/manual/en/function.mb-substr.php)
- [PHP: Regular Expressions - Manual](https://www.php.net/manual/en/regexp.reference.php)