---
date: 2024-01-20 17:55:17.511562-07:00
description: "How to: (\u65B9\u6CD5) PHP\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\
  \u30A4\u30EB\u3092\u8AAD\u3080\u57FA\u672C\u306F\u7C21\u5358\u3067\u3059\u3002`fopen()`\u3001\
  `fgets()`\u3001\u305D\u3057\u3066`fclose()`\u306E\u95A2\u6570\u3092\u4F7F\u3044\u307E\
  \u3059\u3002\u30D5\u30A1\u30A4\u30EB\u5168\u4F53\u3092\u4E00\u5EA6\u306B\u8AAD\u307F\
  \u305F\u3044\u6642\u306F`file_get_contents()`\u304C\u4FBF\u5229\u3067\u3059\u3002\
  \u4F8B\u3092\u898B\u3066\u307F\u307E\u3057\u3087\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.121280-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) PHP\u3067\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\
  \u3092\u8AAD\u3080\u57FA\u672C\u306F\u7C21\u5358\u3067\u3059\u3002`fopen()`\u3001\
  `fgets()`\u3001\u305D\u3057\u3066`fclose()`\u306E\u95A2\u6570\u3092\u4F7F\u3044\u307E\
  \u3059\u3002\u30D5\u30A1\u30A4\u30EB\u5168\u4F53\u3092\u4E00\u5EA6\u306B\u8AAD\u307F\
  \u305F\u3044\u6642\u306F`file_get_contents()`\u304C\u4FBF\u5229\u3067\u3059\u3002\
  \u4F8B\u3092\u898B\u3066\u307F\u307E\u3057\u3087\u3046\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
weight: 22
---

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
