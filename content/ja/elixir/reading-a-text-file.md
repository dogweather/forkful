---
title:                "Elixir: テキストファイルを読む"
simple_title:         "テキストファイルを読む"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

Elixirでテキストファイルを読み取る方法

## Why
テキストファイルを読み取ることは、Elixirプログラミングの重要な部分です。ファイル読み取りを理解することによって、より多くのデータを処理し、より豊富なアプリケーションを作成することができます。

## How To
まずはファイルを開く必要があります。File.open関数を使用してファイルを開くことができます。

```
Elixir
file = File.open("sample.txt")
```

以下のコードブロックでは、ファイル全体を1行ずつ読み取り、出力する方法を示しています。

```
Elixir
File.stream!("sample.txt")
|> Enum.each(fn line ->
  IO.puts line
end)
```
さらに、ファイルをインデックスごとに分割して読み取ることもできます。以下のコードブロックでは、リスト内包表記を使用してファイルを読み取り、出力する方法を示しています。

```
Elixir
file = File.open!("sample.txt")
IO.puts [for i <- 1..5, do: File.get(file, i, :line)]
```

出力結果は以下のようになります。

```
"My name is Sakura."
"Nice to meet you, Sakura."
"How are you, Sakura?"
"I'm doing great."
"What about you?"
```

## Deep Dive
Fileモジュールはさまざまな関数を提供しています。たとえば、ファイルの先頭や末尾から読み取ることができる関数があります。また、ファイルポインターを使用して任意の位置から読み取ることもできます。

さらに、ファイルの内容をパースして、特定のデータを抽出することもできます。例えば、CSVファイルからデータを抽出することができます。また、ファイルへの書き込みやファイルの作成も可能です。

## See Also
- [Fileモジュールのドキュメント](https://hexdocs.pm/elixir/File.html)
- [Elixirでファイルを操作する方法](https://elixirschool.com/jp/lessons/advanced/file-io/)
- [ElixirのファイルI/O](https://www.sitepoint.com/elixir-file-io/)