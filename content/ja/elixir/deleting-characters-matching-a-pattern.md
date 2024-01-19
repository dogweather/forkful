---
title:                "パターンに一致する文字を削除する"
html_title:           "C: パターンに一致する文字を削除する"
simple_title:         "パターンに一致する文字を削除する"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何となぜ？

文字列からパターンに一致する文字を削除するとは、特定の文字列から特定のパターンを見つけ出し、それを取り除くプロセスを指します。プログラマーがこれを行う主な理由は、不要なスペースや特殊文字を削除してデータをクリーンにするためです。

## 方法：

Elixirでこのタスクを実行するためのコードサンプルを見てみましょう。

```Elixir
defmodule Sample do
  def delete_matching_chars(str, pattern) do
    String.replace(str, pattern, "")
  end
end

IO.puts Sample.delete_matching_chars("Hello, World!", ",")
```

上記のスクリプトを実行すると、次の出力が得られます：

```Elixir
Hello World!
```

この例では、我々はカンマを削除しました。

## ディープダイブ：

パターンに一致する文字の削除は、多くのプログラミング言語で実装されている基本的な文字列操作です。これにより、ノイズとなる可能性のあるデータから特定のパターンを効果的に削除できます。また、パターンマッチングでさらに強力な制御を持つことができます。

Elixirの場合、`String.replace/3`関数はこの目的のために使用されます。この関数は、指定されたパターンが見つかった場合に文字列内の該当部分を新しい文字列で置き換えます。パターンが空文字列("")の場合、該当部分は削除されます。

一方、Elixir以外の言語でも似たような機能を提供するものがあります。例えば、JavaScriptには`replace()`メソッド、Rubyには`gsub()`メソッドがあります。

## 参考資料：

以下のリンクでは、Elixirの文字列操作についてさらに詳しく見ることができます。

1. Elixir公式ドキュメンテーションの[Stringモジュール](https://hexdocs.pm/elixir/String.html)
2. Elixir Schoolの[Strings and Character Lists](https://elixirschool.com/jp/lessons/basics/strings-and-characters/)
3. [Elixirでの文字列の使用](https://elixir-lang.jp/getting-started/io-and-the-file-system.html) - 初心者向けのチュートリアル