---
title:                "Elixir: コンピューター・プログラミングにおける「コマンドライン引数の読み取り」"
simple_title:         "コンピューター・プログラミングにおける「コマンドライン引数の読み取り」"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ
コマンドライン引数を読み取ることの重要性は、Elixirプログラミングの基本的なスキルの1つです。コマンドライン引数をうまく活用することで、プログラムを実行する際の柔軟性が増し、よりパワフルなツールを作ることができます。

## 方法
以下は、コマンドライン引数を読み取るための簡単なElixirコードの例です。

```Elixir
# コマンドライン引数を取得する
args = System.argv

# 最初の引数を取得する
first = List.first(args)

# 最後の引数を取得する
last = List.last(args)

# 引数を結合して出力する
IO.puts "入力された引数は#{first}と#{last}です。"
```

上記のコードを実行すると、入力されたコマンドライン引数が表示されます。

```
$ elixir example.exs 引数1 引数2
入力された引数は引数1と引数2です。
```

## ディープダイブ
コマンドライン引数を読み取る際には、`System.argv`以外にも`OptionParser`モジュールを使う方法もあります。`OptionParser`を使うことで、より複雑な引数の解析やオプションの設定が可能になります。

また、コマンドライン引数をうまく活用すると、ユーザーからの入力によってプログラムの動作を変えることができます。これは、動的なプログラムを作る上で重要なスキルです。

## See Also
- [Elixir Command Line Utilities](https://elixir-lang.org/getting-started/command-line.html)
- [Elixir OptionParser Module](https://hexdocs.pm/elixir/OptionParser.html)
- [Command Line Arguments in Elixir](https://www.educative.io/edpresso/command-line-arguments-in-elixir)