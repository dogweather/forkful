---
title:                "コマンドライン引数の読み取り"
html_title:           "Bash: コマンドライン引数の読み取り"
simple_title:         "コマンドライン引数の読み取り"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何となぜ？

コマンドライン引数の読み取りは、ユーザーからの入力情報を受け取るプログラムの方法の一つです。プログラマがこれを行う主な理由は、実行時にプログラムの動作を制御するためです。

## 使い方：

Elixirでコマンドライン引数を読み取るには、`System.argv/0`関数を使用します。以下に簡単な例を示します。

```Elixir
defmodule Test do
  def main(args) do
    IO.inspect(args)
  end
end

System.argv |> Test.main
```

このコードを`test.exs`として保存し、`elixir test.exs arg1 arg2 arg3`というコマンドを使って実行すると、 `["arg1", "arg2", "arg3"]`という出力が得られます。

## ディープダイブ

1. 歴史的文脈：コマンドライン引数はUNIXおよびその他のオペレーティングシステムで一般的に使用され、ツールの柔軟性を提供します。

2. 代替手段：入力を受け取る他の方法としては、標準入力、設定ファイル、環境変数などがあります。それぞれが異なるユースケースに適しています。

3. 実装の詳細：Elixirでは、`System.argv/0`関数はコマンドライン引数をリストとして返します。このリストは文字列のリストであり、各引数がそのままの形でリストに格納されます。

## 関連情報

1. [Elixir公式ドキュメンテーション - System.argv/0](https://hexdocs.pm/elixir/System.html#argv/0)
 
2. [Elixir School - コマンドライン引数](https://elixirschool.com/jp/lessons/basics/io/)