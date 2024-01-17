---
title:                "「一時ファイルの作成」"
html_title:           "Elixir: 「一時ファイルの作成」"
simple_title:         "「一時ファイルの作成」"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## やり方と意義？
一時ファイルを作成することは、プログラマーにとって何かを一時的に保存することができる便利な方法です。例えば、データベースへの一時的なバックアップや、一時的な処理に使用するファイルを作成することができます。プログラマーが一時ファイルを作成する理由は、作業中のデータをプログラムが使用できる形式で保存することができるためです。

## やり方：
```
Elixir File.tempfile!

# 一時ファイルを作成し、変数に保存する
{:ok, file} = File.tempfile!
# 一時ファイルにデータを書き込む
IO.write(file, "This is a temporary file")
# 一時ファイルを閉じる
File.close(file)
```

## 詳細な情報：
一時ファイルの発展的なコンテキストとしては、古いオペレーティングシステムでの "swap space" と呼ばれるメモリの利用方法があります。これは、主記憶装置 (RAM) を使用するよりも低速なディスクドライブを使用して、一時的なデータの保存に使用するものでした。また、一時ファイルの代替手段としては、プログラム内でメモリを直接操作する方法や、一時データベースの使用が考えられます。一時ファイルの実装の詳細としては、一時ファイルとそのファイル名を管理するための一意の識別子を使用する方法が挙げられます。

## 関連リンク：
- https://hexdocs.pm/elixir/File.html#tempfile!/0
- https://en.wikipedia.org/wiki/Swap_space
- https://www.techopedia.com/definition/4632/swap-space