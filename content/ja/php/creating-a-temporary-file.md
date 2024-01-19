---
title:                "一時ファイルの作成"
html_title:           "Elixir: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 何となぜ？

一時的なファイルを作成するとは、プログラムが短時間だけ使用するファイルを作ろうとする際の行為を指します。これは通常、大量のデータを一時的に保存する場合や、ファイル共有で一時的なバッファが必要な場合に行われます。

## 方法：

PHPには、一時ファイルを簡単に作成できる関数が用意されています。ここでその具体的なコードを見てみましょう。

```PHP
<?php
$temp_file = tmpfile();

fwrite($temp_file, "This is some cool data.");
fseek($temp_file, 0); 

echo fread($temp_file, 1024); 
?>
```

出力:

```PHP
This is some cool data.
```

このコードはPHPの`tmpfile()`関数を使用していて、一時的なファイルを作成し、そのファイルディスクリプタを返しています。次に、この一時的なファイルにデータを書き込み、読み出しています。

## 深掘り：

歴史的な文脈から見ると、一時的なファイルはUNIXの初期から存在していました。これがPHPに組み込まれたのは、PHP開発者がこのような一時的なデータストレージニーズに対応するためです。

適用する代替策の一つとして、`tempnam()`関数があります。この関数も一時ファイルを作成しますが、ファイルの位置をより詳細にコントロールできます。

詳細な実装については、`tmpfile()`関数はメタデータとしてシステムの/tmpディレクトリを使用します。スクリプトが終了した時点で、この一時ファイルは自動的にシステムから削除されます。

## 関連情報：

以下のリンクでは一時ファイルの関連情報をより詳細に確認できます。

1. PHP公式ドキュメント: [tmpfile](https://www.php.net/manual/ja/function.tmpfile.php)

2. PHP公式ドキュメント: [tempnam](https://www.php.net/manual/ja/function.tempnam.php)