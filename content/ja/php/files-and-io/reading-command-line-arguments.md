---
date: 2024-01-20 17:56:46.685171-07:00
description: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3068\u306F\u3001\
  \u30D7\u30ED\u30B0\u30E9\u30E0\u3092\u8D77\u52D5\u3059\u308B\u969B\u306B\u6E21\u3055\
  \u308C\u308B\u8FFD\u52A0\u60C5\u5831\u306E\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u8AAD\u307F\u8FBC\u307F\u3001\u30B9\
  \u30AF\u30EA\u30D7\u30C8\u306E\u6319\u52D5\u3092\u30AB\u30B9\u30BF\u30DE\u30A4\u30BA\
  \u3059\u308B\u305F\u3081\u306B\u4F7F\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.268334-06:00'
model: gpt-4-1106-preview
summary: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3068\u306F\u3001\
  \u30D7\u30ED\u30B0\u30E9\u30E0\u3092\u8D77\u52D5\u3059\u308B\u969B\u306B\u6E21\u3055\
  \u308C\u308B\u8FFD\u52A0\u60C5\u5831\u306E\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3053\u308C\u3092\u8AAD\u307F\u8FBC\u307F\u3001\u30B9\
  \u30AF\u30EA\u30D7\u30C8\u306E\u6319\u52D5\u3092\u30AB\u30B9\u30BF\u30DE\u30A4\u30BA\
  \u3059\u308B\u305F\u3081\u306B\u4F7F\u3044\u307E\u3059\u3002."
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
weight: 23
---

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
