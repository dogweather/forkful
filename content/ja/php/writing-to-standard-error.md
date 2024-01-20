---
title:                "標準エラーへの書き込み"
html_title:           "Arduino: 標準エラーへの書き込み"
simple_title:         "標準エラーへの書き込み"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (なぜかとは？)
標準エラーに書き込むことは、プログラムがエラーや診断メッセージを出力する方法です。これは出力（STDOUT）と分けることで、エラーログを保守しやすく、システムやファイルに不要なエラーメッセージを記録させないために行われます。

## How to: (やり方)
```PHP
<?php
// 標準出力への書き込み
fwrite(STDOUT, "正常な出力。\n");

// 標準エラーへの書き込み
fwrite(STDERR, "エラーメッセージ。\n");
?>
```
**実行結果**
```
正常な出力。
エラーメッセージ。
```

## Deep Dive (詳細情報)
PHPは初期からストリームラッパーとして`stdout`と`stderr`を持っています。組み込み定数`STDOUT`と`STDERR`はそれぞれ標準出力、標準エラーを指し、`fopen()`や`file_get_contents()`などの関数では使えません。書き込みは`fwrite()`や`file_put_contents()`などを使います。`error_log()`関数を使うこともできますが、これは設定に依存します。

## See Also (関連情報)
- PHP Manual on Standard I/O streams: [https://www.php.net/manual/en/features.commandline.io-streams.php](https://www.php.net/manual/en/features.commandline.io-streams.php)
- PHP Manual on `error_log()`: [https://www.php.net/manual/en/function.error-log.php](https://www.php.net/manual/en/function.error-log.php)
- More about POSIX standard streams: [https://en.wikipedia.org/wiki/Standard_streams](https://en.wikipedia.org/wiki/Standard_streams)