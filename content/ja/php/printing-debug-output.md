---
title:                "PHP: デバッグ出力の印刷"
programming_language: "PHP"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜデバッグ出力をする必要があるのか

プログラミングに慣れている人には、デバッグ出力がなぜ重要なのか、簡単に理解ができるかもしれません。しかし、初心者の方にとっては、なかなか理解しにくいかもしれません。デバッグ出力をすることで、コードの実行中にどのような値が取得されているかを確認することができます。エラーがあった場合にも、どこで発生しているかを特定することができます。また、コードの複雑な部分を理解するのにも役立ちます。 

## デバッグ出力の方法
プログラミングでは、変数や配列の中身を確認するためにデバッグ出力が必要です。PHPでは、次のように`print_r()`関数を使用することで、変数の中身を出力することができます。

```PHP
$fruit = ["apple", "orange", "banana"];

print_r($fruit);
```

上記のコードを実行すると、次のような出力が得られます。

```
Array
(
    [0] => apple
    [1] => orange
    [2] => banana
)
```

また、変数の値を確認するだけでなく、プログラムの実行フローを追跡するためにもデバッグ出力は役立ちます。PHPには`var_dump()`関数もあります。次のように使用することで、変数の値だけでなく、データ型や値の長さも確認することができます。

```PHP
$num = 5;
$str = "Hello";

var_dump($num, $str);
```

出力結果:

```
int(5)
string(5) "Hello"
```

## デバッグ出力の深堀り

PHPのデバッグ出力には、他にも多くの関数が用意されています。例えば、`debug_backtrace()`関数は、実行された関数やファイルの情報を取得するのに役立ちます。また、`error_get_last()`関数は直近のエラー情報を取得することができます。これらの関数を組み合わせることで、より詳細なデバッグ情報を取得することができます。

## 併せて読みたい記事

- [PHP公式ドキュメント：デバッグ出力](https://www.php.net/manual/ja/function.print-r.php)  
- [PHP公式ドキュメント：var_dump()関数](https://www.php.net/manual/ja/function.var-dump.php)  
- [PHP公式ドキュメント：debug_backtrace()関数](https://www.php.net/manual/ja/function.debug-backtrace.php)