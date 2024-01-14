---
title:                "PHP: 文字列の大文字化"
simple_title:         "文字列の大文字化"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

コンピュータープログラミングを学ぶと、さまざまな種類の問題を解決できます。文字列を操作することもその1つです。文字列を大文字に変換する必要がある場合は、PHPプログラムでそれを簡単に実行することができます。

## 方法

文字列を大文字に変換するには、PHPの組み込み関数である`strtoupper()`を使用します。この関数は、与えられた文字列のすべての文字を大文字に変換することができます。以下の例をご覧ください。

```PHP
$string = "hello world";
echo strtoupper($string); // 出力: HELLO WORLD
```

また、`strtoupper()`を使用して、一部の文字のみを大文字に変換することもできます。対象となる文字を大文字の配列として渡すだけです。例えば、以下のコードでは最初の文字のみを大文字に変換しています。

```PHP
$string = "hello world";
echo ucfirst($string); // 出力: Hello world
```

## ディープダイブ

文字列を大文字に変換する際、PHPの組み込み関数では、アルファベット以外の文字にも対応しています。例えば、日本語の文字列を大文字に変換することも可能です。しかし、この場合は言語によって大文字に変換する方法が異なるため、注意が必要です。詳しくは、PHPの公式ドキュメントを参照してください。

## さらに見る

- PHPの公式ドキュメント：https://www.php.net/manual/ja/function.strtoupper.php
- 文字列操作に関するチュートリアル：https://www.tutorialspoint.com/php/php_strings.htm
- 日本語の文字列を大文字に変換する方法についての詳しい説明：https://pleiades.io/help/php/function.strtoupper
- 文字列を大文字に変換する他の方法：https://www.php.net/manual/ja/language.types.string.php#language.types.string.case-conversion