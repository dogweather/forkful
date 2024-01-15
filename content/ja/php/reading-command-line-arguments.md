---
title:                "コンピュータープログラミングの記事のタイトル:「コマンドライン引数を読む」"
html_title:           "PHP: コンピュータープログラミングの記事のタイトル:「コマンドライン引数を読む」"
simple_title:         "コンピュータープログラミングの記事のタイトル:「コマンドライン引数を読む」"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜCLI引数を読む必要があるのか

CLI引数を読むことは、ユーザーから入力を受け取るために非常に便利です。例えば、スクリプトを起動するときに様々なオプションやパラメーターを指定できます。これにより、より柔軟なプログラムを作成することができます。

## CLI引数を読む方法

CLI引数を読むには、`$argv`配列を使用します。以下のコードを参考にしてください。

```PHP
<?php
// 引数が渡されたかを確認
if (isset($argv[1])) {
    // 第1引数を出力
    echo "Hello, " . $argv[1] . "!";
} else {
    echo "Hello, world!";
}

// 結果例：
// $ php script.php John
// Hello, John!
// $ php script.php
// Hello, world!
```

## 深堀り

CLI引数には、長いオプション名と短いオプション名の両方を指定することができます。長いオプション名は`--`で始まる文字列で指定します。短いオプション名は`-`で始まる文字で指定します。

また、CLI引数をパースするために`getopt()`関数を使用することもできます。これにより、引数の順序に依存せずにオプションが読み取られます。以下のコードを参考にしてください。

```PHP
<?php
// オプションやパラメーターを指定
$options = getopt("l:n:");

// オプションが指定された場合は値を出力
if (isset($options['l'])) {
    echo "Language: " . $options['l'];
}

// パラメーターが指定された場合は値を出力
if (isset($options['n'])) {
    echo "Name: " . $options['n'];
}

// 結果例：
// $ php script.php -l PHP -n John
// Language: PHP
// Name: John
```

## 他に見るべきもの

- [PHP CLI引数ドキュメント](https://www.php.net/manual/ja/features.commandline.php)
- [getopt()関数ドキュメント](https://www.php.net/manual/ja/function.getopt.php)