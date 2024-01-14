---
title:                "PHP: 文字列の長さを見つける"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の長さを求めることの利点について説明します。文字列の長さを知ることは、プログラミングにおいて非常に重要な情報です。例えば、文字列の長さを知ることで、テキストを正しくフォーマットすることができ、必要なメモリーの量を最適化することができます。

## 方法

文字列の長さを求めるための基本的な方法は、PHPの組み込み関数である`strlen()`を使用することです。以下のように`strlen()`を使用して、文字列`Hello World!`の長さを求めることができます。

```PHP
$str = "Hello World!";
echo strlen($str);
```

上記のコードを実行すると、次の出力が得られます。

```
12
```

文字列の中に日本語や特殊文字が含まれる場合、`strlen()`は正しい長さを返さないことがあります。そのような場合は、マルチバイト文字列も扱える`mb_strlen()`を使用しましょう。下記のように使用することができます。

```PHP
$str = "こんにちは、世界！";
echo mb_strlen($str);
```

上記のコードを実行すると、次の出力が得られます。

```
9
```

また、文字列に含まれる単語の数を求めるには、`str_word_count()`を使用することもできます。下記のように使用することができます。

```PHP
$str = "Today is a beautiful day.";
echo str_word_count($str);
```

上記のコードを実行すると、次の出力が得られます。

```
5
```

## ディープダイブ

`strlen()`による文字列の長さの取得は、非常に基本的な操作ですが、その実装には少し興味深い点があります。実際には、`strlen()`は文字列のバイト数を返しており、日本語や特殊文字が含まれる場合、文字数とは必ずしも一致しません。これは、日本語や特殊文字が複数のバイトで表現されるためです。そのため、真の意味での文字数を取得するには、`mb_strlen()`のようなマルチバイト対応の関数を使用する必要があります。

## それ以外にも興味がある方は...

- [PHPマニュアル - strlen()関数](https://www.php.net/manual/ja/function.strlen.php)
- [PHPマニュアル - mb_strlen()関数](https://www.php.net/manual/ja/function.mb-strlen.php)
- [PHPマニュアル - str_word_count()関数](https://www.php.net/manual/ja/function.str-word-count.php)