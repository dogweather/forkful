---
title:                "テキストファイルの書き込み"
date:                  2024-01-19
html_title:           "Bash: テキストファイルの書き込み"
simple_title:         "テキストファイルの書き込み"

category:             "PHP"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ?)
テキストファイルの書き込みとは、データをテキスト形式でファイルに保存するプロセスです。データを永続化し、後で再利用可能にするためにプログラマーはこれを行います。

## How to: (方法)
```PHP
<?php
$text = "こんにちは世界\n"; // 書き込む内容
$file = fopen("example.txt", "w"); // ファイルを開く

if ($file) {
    fwrite($file, $text); // ファイルにテキストを書き込む
    fclose($file); // ファイルを閉じる
} else {
    echo "ファイルを開けませんでした。";
}
?>
```
出力: `example.txt` に "こんにちは世界" というテキストが保存されます。

## Deep Dive (深い情報)
PHPでは、`fopen()`, `fwrite()`, `fclose()` 関数を使用してファイル操作を行います。これはPHP 4から利用可能で、基本的なファイル操作のための標準的な手法です。代替として、`file_put_contents()` 関数もあり、一行でファイル書き込みを実行できます。実装においては、ファイルのオープンモードを適切に選ぶ（読み取り、書き込み、追加など）のが重要です。

## See Also (関連情報)
- [PHP Manual - Filesystem Functions](https://www.php.net/manual/ja/ref.filesystem.php)
- [W3Schools - PHP File Handling](https://www.w3schools.com/php/php_file.asp)
