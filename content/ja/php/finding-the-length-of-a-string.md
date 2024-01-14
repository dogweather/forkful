---
title:    "PHP: 文字列の長さを見つける"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

「なぜ」：文字列の長さを調べることに興味を持つ理由を1-2文で説明します。

「方法」：「```PHP ...```」のコードブロック内のコーディング例とサンプルの出力。

「深堀り」：文字列の長さを調べる際の詳細な情報。

「参考リンク」：この記事と関連するリンクのリスト。 

##なぜ

文字列の長さを調べることは、プログラミングにおいて非常に重要なタスクです。例えば、データの入力や処理の際、文字列の長さを確認する必要がある場合があります。また、文字列の長さを正確に把握することで、データの整合性を保つことができます。

##方法

文字列の長さを調べるには、PHPの組み込み関数である`strlen()`を使用します。この関数は、与えられた文字列の長さを整数で返します。例えば、下記のコードを使用すると、文字列「こんにちは、世界！」の長さを知ることができます。

```PHP
<?php

$string = "こんにちは、世界！";
echo strlen($string);

//出力結果：10
```

##深堀り

文字列の長さを調べる際には、マルチバイト文字やUTF-8文字列に注意する必要があります。これらの文字は1文字あたりのバイト数が異なるため、`strlen()`関数を使用すると正確な結果が得られません。その場合は代わりに、`mb_strlen()`関数を使用することで正しい長さを取得することができます。

また、文字列内に特定の文字や文字列が何回出現するかを調べる場合には、`substr_count()`関数を使用することで簡単にカウントすることができます。

##参考リンク

- PHPの`strlen()`関数について: https://www.php.net/manual/en/function.strlen.php
- マルチバイト文字と`mb_strlen()`関数: https://www.php.net/manual/en/function.mb-strlen.php
- `substr_count()`関数の使用方法: https://www.php.net/manual/en/function.substr-count.php