---
title:                "PHP: デバッグ出力の印刷"
simple_title:         "デバッグ出力の印刷"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜデバッグ出力を行うのか

デバッグ出力は、プログラムのエラーを特定するための重要なテクニックです。エラーが発生した際にコード内の特定の場所で変数や処理の値を出力することで、問題を特定し解決することができます。

## デバッグ出力の方法

デバッグ出力は、```print_r()```や```var_dump()```などのPHPの組み込み関数を使用することで簡単に実現することができます。例えば、以下のように変数やオブジェクトを出力することができます。

```PHP
$number = 10;
$obj = new stdClass();
$obj->name = "John";
print_r($number);
var_dump($obj);
```

すると、以下のような出力が得られます。

```PHP
10
object(stdClass)#1 (1) {
  ["name"]=>
  string(4) "John"
}
```

このように、変数の値やオブジェクトの構造を見ることで、エラーの原因を特定することができます。

## デバッグ出力の詳細

実際にデバッグ出力を行った際に、出力結果の意味を正しく理解することも重要です。PHPのドキュメントやオンラインのリソースを参考にして、出力結果がどのような意味を持つのかを学ぶことができます。また、デバッグ出力を行う際には、必要な部分のみを出力するようにコーディングすることも重要です。あまりにも多くの情報を出力すると、逆に問題の特定が難しくなってしまいます。

## さらに見る

参考になるリソースや関連するリンクを以下にまとめました。

- PHPの```print_r()```関数のドキュメント：https://www.php.net/manual/en/function.print-r.php
- PHPの```var_dump()```関数のドキュメント：https://www.php.net/manual/en/function.var-dump.php
- PHPのデバッガーXdebugのドキュメント：https://xdebug.org/docs/
- PHPストレージデバッガーのドキュメント：https://github.com/maximebf/php-debug/blob/master/README.md