---
title:                "コマンドライン引数の読み取り"
html_title:           "Bash: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何となぜ？

コマンドライン引数の読み取りは、プログラムが起動する際に外部から受け取ったデータの管理を意味します。これを行うことにより、コードの柔軟性と再利用性が向上し、開発者の作業がスムーズになります。

## どうやって：

以下はPHPでコマンドライン引数を読み取る方法のサンプルコードです：

```PHP
<?php
if ($argc > 1) {
    echo $argv[1];
} else {
echo "No arguments provided.";
}
?>
```
このコードの出力例は以下の通りです：
当該PHPスクリプトを`test.php argument1`で実行した場合、`argument1`が出力されます。一方、引数なしで実行した場合、`No arguments provided.`が出力されます。

## ディープダイブ：

歴史的文脈において、コマンドライン引数はUNIXシステムで広く採用されてきました。これにより、コリアビリティ（パイプラインの組み立て）が可能になりました。

代替として、環境変数を読み込む方法もありますが、これは一時的なデータ、すなわち一回限りのデータをパスする際に便利です。

PHPでは、$argcと$argvという特別な変数を用いてコマンドライン引数を取得します。$argcはコマンドライン引数の数を、$argvはそれらの引数自体を表します。

## 参考：

1. PHP公式ドキュメンテーション：http://php.net/manual/ja/reserved.variables.argv.php
2. PHPコマンドライン引数チュートリアル：https://www.php.net/manual/ja/features.commandline.php