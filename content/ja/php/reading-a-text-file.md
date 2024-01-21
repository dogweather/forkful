---
title:                "テキストファイルの読み込み"
date:                  2024-01-20T17:55:17.511562-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストファイルの読み込み"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (なにを？どうして？)
テキストファイル読み込みは、文字が記録されたファイルを読むことです。プログラマはデータを扱ったり、設定を読み込んだりするためにこの操作を行います。

## How to: (方法)
PHPでテキストファイルを読む基本は簡単です。`fopen()`、`fgets()`、そして`fclose()`の関数を使います。ファイル全体を一度に読みたい時は`file_get_contents()`が便利です。例を見てみましょう。

```PHP
<?php
// ファイルを開く
$file = fopen("example.txt", "r");

// ファイルが正常に開いたかをチェック
if ($file) {
    // 一行ずつファイルを読む
    while (($line = fgets($file)) !== false) {
        echo $line;
    }
    // ファイルを閉じる
    fclose($file);
} else {
    // エラー処理
    echo "ファイルを開けませんでした。";
}

// もっと簡単にファイル全体を読む
$content = file_get_contents("example.txt");
echo $content;
?>
```

サンプルの出力:

```
こんにちは、これはテキストファイルです。
次の行です。
```

## Deep Dive (深堀り)
昔は、ファイル読み込みはI/O操作で重要な部分でしたが、今でもその真価は変わりません。`fopen()`、`fgets()`、`fclose()`はC言語の標準ライブラリの関数を踏襲しています。しかも、PHPでのファイル読み込みには代替方法もあります。例えば、`file()`関数はファイルを読み、配列として各行を返します。`readfile()`関数はファイルの内容を読み出し、それを直接出力します。

パフォーマンスに関してですが、大きなファイルを扱う場合はメモリの消費を考えて、`fgets()`を使って一行ずつ読むか、`fread()`を使って特定のバイト数だけ読む方法が賢明です。

## See Also (関連情報)
- PHP公式ドキュメントのファイル関数に関するセクション: [PHP: Filesystem Functions](https://www.php.net/manual/en/ref.filesystem.php)
- より進んだファイル操作のための`SplFileObject`クラス: [PHP: SplFileObject](https://www.php.net/manual/en/class.splfileobject.php)