---
title:                "一時ファイルの作成"
html_title:           "Elixir: 一時ファイルの作成"
simple_title:         "一時ファイルの作成"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## 何となぜ？
一時ファイルの作成とは、一時的にデータを保存するための短期的な記憶方法です。これは、大量のデータを扱う、または永続性が必要でないときにプログラマーが行います。

## 実施方法：
Elixirで一時ファイルを作成する例を示します。 `File` モジュールを使用して、以下のように行います。

```Elixir
{:ok, file} = File.open("temp.txt", [:write])
File.write!(file, "一時ファイルへのデータ書き込み")
File.read!("temp.txt")
File.close(file)
```
出力:
```
"一時ファイルへのデータ書き込み"
```

その後、不要になった時点でファイルを削除します。

```Elixir
File.rm!("temp.txt")
```

## ディープダイブ:
一時ファイルはUNIX系のオペレーティング・システムで初めて普及し、プログラマがファイルシステムに一時的なデータを書き込むための手段として利用されました。

一時ファイルを作成する古典的な方法以外にも、状況に応じて複数の代替手段が存在します。例えば、ETS(エルラングタームストレージ)を使用すると、ディスク上に書き込むことなくメモリ上でデータを一時的に保存できます。

Elixirでは、`:write` の代わりに `:ram` を指定すれば、データはRAMに保存されます。ただし、この方法を使用すると、アプリケーションがシャットダウンするときにデータが失われます。

## 関連情報:
- [Elixir公式ドキュメンテーション:Fileモジュール](https://hexdocs.pm/elixir/File.html)
- [ETSについての詳細](http://erlang.org/doc/man/ets.html)