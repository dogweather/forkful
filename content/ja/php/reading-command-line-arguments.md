---
title:                "コンピュータプログラミングの記事タイトル：コマンドライン引数の読み取り"
html_title:           "PHP: コンピュータプログラミングの記事タイトル：コマンドライン引数の読み取り"
simple_title:         "コンピュータプログラミングの記事タイトル：コマンドライン引数の読み取り"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何？なぜ？
コマンドライン引数を読み取ることは、PHPプログラマーにとって非常に重要です。コマンドライン引数とは、PHPスクリプトに渡されるコマンドライン上のパラメーターのことです。プログラマーはこれを行うことで、ユーザーからの入力を受け取り、それに基づいてスクリプトの動作を制御することができます。

## 方法：
PHPの```argv```変数を使用して、コマンドライン引数を読み取ることができます。以下の例では、コマンドライン上で指定した数値を加算するプログラムを作成します。

```php
// PHPスクリプトを実行する際、引数を指定します
// 例：php add.php 3 4

<?php
// コマンドライン引数を読み取る
$num1 = $argv[1];
$num2 = $argv[2];
// 加算する
$result = $num1 + $num2;
echo "Result: " . $result . "\n";
?>
```

上記の例では、コマンドライン上で```php add.php 3 4```というコマンドを実行することで、結果として```Result: 7```が出力されます。

## 詳細：
コマンドライン引数を読み取る方法はPHP以外にもあります。例えば、```$_SERVER['argv']```を使用することもできます。また、コマンドラインオプションを解析するためのライブラリも存在します。

PHPでは、コマンドライン引数が```argv```変数として提供されます。この変数には、コマンドライン上の引数が配列として格納されます。最初の要素には実行したスクリプト名が含まれ、その後の要素にはコマンドライン上で指定した引数が順番に格納されます。

## 関連情報：
- [PHP: コマンドラインサポート](https://www.php.net/manual/ja/features.commandline.php)
- [PHP: 配列](https://www.php.net/manual/ja/language.types.array.php)
- [PHP: コマンドラインオプション解析](https://www.php.net/manual/ja/function.getopt.php)