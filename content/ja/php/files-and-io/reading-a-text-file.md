---
date: 2024-01-20 17:55:17.511562-07:00
description: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u8AAD\u307F\u8FBC\u307F\
  \u306F\u3001\u6587\u5B57\u304C\u8A18\u9332\u3055\u308C\u305F\u30D5\u30A1\u30A4\u30EB\
  \u3092\u8AAD\u3080\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\
  \u30C7\u30FC\u30BF\u3092\u6271\u3063\u305F\u308A\u3001\u8A2D\u5B9A\u3092\u8AAD\u307F\
  \u8FBC\u3093\u3060\u308A\u3059\u308B\u305F\u3081\u306B\u3053\u306E\u64CD\u4F5C\u3092\
  \u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.407165
model: gpt-4-1106-preview
summary: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u8AAD\u307F\u8FBC\u307F\
  \u306F\u3001\u6587\u5B57\u304C\u8A18\u9332\u3055\u308C\u305F\u30D5\u30A1\u30A4\u30EB\
  \u3092\u8AAD\u3080\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\
  \u30C7\u30FC\u30BF\u3092\u6271\u3063\u305F\u308A\u3001\u8A2D\u5B9A\u3092\u8AAD\u307F\
  \u8FBC\u3093\u3060\u308A\u3059\u308B\u305F\u3081\u306B\u3053\u306E\u64CD\u4F5C\u3092\
  \u884C\u3044\u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
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
