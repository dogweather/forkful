---
title:    "Elixir: テキストファイルの読み込み"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## なぜ

テキストファイルを読むのに、わざわざブログ記事を読む必要はあるのでしょうか？それは、Elixirプログラマーとしてより効率的かつ効果的に作業するためです。テキストファイルを読むためのElixirの構文を学ぶことで、単純な作業から複雑なデータ処理まで、さまざまなタスクをより簡単に実行することができます。

## 使い方

Elixirでファイルを読み込む方法を学ぶために、まずは```File.read```関数を使ってテキストファイルを読み込む方法を見てみましょう。

```Elixir
file = File.read("sample.txt") # テキストファイルを読み込む
IO.puts(file) # ファイルの中身を出力する
```

上記のコードを実行すると、```sample.txt```ファイルの内容がターミナルに表示されます。ファイルの内容を取得したい場合は、```File.read!```関数を使ってファイルの中身を文字列として取得することもできます。

また、Elixirではさまざまな方法でテキストファイルを読み込むことができます。例えば、CSVファイルを読み込んでデータを整形することもできます。

```Elixir
File.stream!("data.csv") # CSVファイルをストリームとして読み込む
|> CSVParser.parse_stream() # データをパースする
|> Enum.map(fn [name, age] -> {name, String.to_integer(age)} end) # データを整形する
|> Enum.sort() # データをソートする
|> Enum.each(fn {name, age} -> IO.puts("#{name} is #{age} years old") end) # データを出力する
```

このように、Elixirではテキストファイルのデータを柔軟に扱うことができます。

## ディープダイブ

テキストファイルを読み込む際には、ファイルがどのエンコーディングで作られているかに注意する必要があります。デフォルトでは、ElixirはUTF-8エンコーディングを使用しますが、必要に応じて他のエンコーディングを指定することもできます。

また、ファイルの中の行を指定して特定の箇所のみを読み込むこともできます。例えば、```File.read("sample.txt", 3, 5)```とすると、ファイルの3行目から5行目までの内容を読み込むことができます。

さらに、Elixirには便利な機能であるパイプラインもあります。パイプラインを使うことで、テキストファイルを読み込む際にさまざまな処理を行うことができます。例えば、ファイルを読み込んだ後に正規表現を使って文字列のマッチングを行うこともできます。

## その他の参考リンク

- [Elixirの公式ドキュメント](https://elixir-lang.org/getting-started/introduction.html)
- [テキストファイルを扱うためのElixirライブラリ](https://hexdocs.pm/elixir/1.12/File.html)
- [Elixirのパイプラインについての詳細](https