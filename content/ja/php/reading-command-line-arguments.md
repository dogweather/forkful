---
title:                "PHP: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# なぜコマンドライン引数を読み取るのか

コマンドライン引数は、プログラミング言語でよく使用される重要な機能です。これを使用することで、プログラムをより柔軟に制御することができます。例えば、プログラムに必要な設定やパラメーターを動的に指定することができます。また、コマンドライン引数を用いることで、プログラムを実行する際の手間を省くことができます。

# コマンドライン引数の読み取り方

PHPでコマンドライン引数を読み取るには、`$argv`という特別な配列を使用します。この配列には、プログラムを実行する際に指定したすべての引数が含まれています。以下のコードブロックを参考にしてください。

```PHP
<?php
// ファイル名を除いたコマンドライン引数の数を取得する
$count = count($argv) - 1;
// 引数が指定されていない場合はエラーメッセージを表示する
if ($count < 1) {
    exit("引数が必要です。");
}
// コマンドライン引数を取得して出力する
for ($i = 0; $i < $count; $i++) {
    // $argv[0]にはファイル名が含まれるため、+1した値を使用する
    echo "引数" . ($i + 1) . ": " . $argv[$i + 1] . "\n";
}
```

実行時に、`php script.php argument1 argument2 argument3`のように引数を指定すると、以下のような出力が得られます。

```
引数1: argument1
引数2: argument2
引数3: argument3
```

# コマンドライン引数の詳細

コマンドライン引数を読み取る際には、`$argv`の他にも`$argc`という特別な変数が使用されます。`$argc`には、コマンドライン引数の数を示す整数値が格納されます。また、`$argv`の各要素は文字列として扱われるため、数字やブール値を取得する際には適切な型変換が必要です。

# おわりに

コマンドライン引数を読み取る方法について学びました。この機能を上手に活用することで、プログラムをより柔軟に構築することができます。ぜひ実践してみてください！

# 関連リンク

- [PHPの公式ドキュメント: コマンドライン実行時の引数](https://www.php.net/manual/ja/reserved.variables.argv.php)
- [「PHPプログラミング」: コマンドライン引数の取得方法](https://www.php.net/manual/ja/reserved.variables.argv.php)
- [Qiita: PHPでコマンドライン引数を使用する方法](https://qiita.com/ukiuni@github/items/8373989e1afc5f3291bb)