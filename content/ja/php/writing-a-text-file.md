---
title:                "PHP: テキストファイルの作成"
simple_title:         "テキストファイルの作成"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/writing-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

テキストファイルを書く理由は何でしょうか。 まず、テキストファイルは非常にシンプルで使いやすい形式です。 また、データベースやスプレッドシートなどのより複雑なファイル形式よりも、扱いやすい場合が多いです。

## 方法

テキストファイルを書くには、PHPを使用することができます。 下のコードブロックを見てみましょう。

```PHP
<?php
// テキストファイルの名前を指定する
$file = 'sample.txt';

// ファイルを書き込みモードで開く
$handle = fopen($file, 'w');

//書き込む内容を指定する
$content = 'これはテストです。';

// ファイルに内容を書き込む
fwrite($handle, $content);

// ファイルを閉じる
fclose($handle);
?>
```

上記のコードを実行すると、`sample.txt`という名前のテキストファイルに「これはテストです。」という内容が書き込まれます。

## ディープダイブ

テキストファイルを書くときには、ファイルのエンコーディングにも注意を払う必要があります。 日本語のテキストファイルを書く場合には、UTF-8エンコーディングを指定することで、文字化けを防ぐことができます。

また、テキストファイルを読み込む際には、`fopen`関数の第二引数に`r`を指定することで、ファイルを読み込みモードで開くことができます。 さらに、`fread`関数を使用することで、ファイルの内容を読み込むことができます。

## 参考リンク

- [PHPの公式ドキュメント](https://www.php.net/manual/ja/function.fopen.php)
- [文字コードとは何か](https://wa3.i-3-i.info/word12304.html)
- [PHPでテキストファイルを読み込む方法](https://www.flatflag.nir87.com/file-text-read/)