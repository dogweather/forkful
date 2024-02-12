---
title:                "コマンドライン引数の読み取り"
aliases:
- /ja/php/reading-command-line-arguments/
date:                  2024-01-20T17:56:46.685171-07:00
model:                 gpt-4-1106-preview
simple_title:         "コマンドライン引数の読み取り"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
コマンドライン引数とは、プログラムを起動する際に渡される追加情報のことです。プログラマーはこれを読み込み、スクリプトの挙動をカスタマイズするために使います。

## How to (方法)
PHPでは`$argv`と`$argc`変数を使って引数にアクセスできます。例を見てみましょう。

```php
<?php
// 引数のカウント
$numberOfArgs = $argc - 1; // 最初の引数はスクリプト名なので除外

echo "引数の数: " . $numberOfArgs . "\n";

// 引数の詳細を表示
foreach ($argv as $index => $arg) {
    if ($index === 0) continue; // スクリプト名は飛ばす
    echo "引数 " . $index . ": " . $arg . "\n";
}
?>
```

Terminalからこのスクリプトを実行するとこんな感じです。

```bash
$ php script.php 日本 Tokyo 2023
引数の数: 3
引数 1: 日本
引数 2: Tokyo
引数 3: 2023
```

## Deep Dive (深掘り)
最初に、`$argv`と`$argc`が出てきたのはPHP 4.3.0バージョンからです。これらはフラグで有効にする必要がありましたが、PHP 5.3.0以降、デフォルトで使用可能になりました。

他の方法として、`getopt`関数もあります。こちらはコマンドラインオプションをより詳細に解析します。

実装の上では、PHPはコマンドラインからスクリプトを呼び出すシェルに依存しています。`$argv`は単にそのシェルから渡された文字列の配列です。

## See Also (参照)
- [PHP公式マニュアル: $argv](https://www.php.net/manual/ja/reserved.variables.argv.php)
- [PHP公式マニュアル: $argc](https://www.php.net/manual/ja/reserved.variables.argc.php)
- [PHP公式マニュアル: getopt](https://www.php.net/manual/ja/function.getopt.php)
