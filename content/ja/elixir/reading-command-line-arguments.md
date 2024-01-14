---
title:                "Elixir: コンピュータープログラミングの記事タイトル：コマンドライン引数の読み取り"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##Why
なぜ読み込みコマンドライン引数について学ぶべきなのでしょうか？コマンドライン引数はコマンドラインからプログラムに情報を渡すことを可能にします。例えば、プログラムにファイルを開くかどうかを指定するために、ファイルのパスを引数として渡すことができます。

##How To
Elixirでは、コマンドライン引数を`System.argv`から取得することができます。例えば、以下のコードを実行すると、コマンドライン引数のリストが返されます。

```Elixir
IO.inspect System.argv
```

もしコマンドライン引数がある場合は、以下のような出力が得られるでしょう。

```Elixir
["program_name", "argument1", "argument2"]
```

このように、`System.argv`は文字列のリストを返します。そして、リストの最初の要素には実行されているプログラムの名前が含まれています。

##Deep Dive
コマンドライン引数を扱う際に注意すべき点がいくつかあります。まず、引数の数をチェックすることが重要です。もし必要な引数の数よりも少ない場合は、エラーを返すようにプログラムを設計する必要があります。また、引数には数値や文字列以外にも、オプションフラグなどの特殊な形式の引数が含まれることがあります。その場合は、正しく処理するようにコードを書く必要があります。

さらに、コマンドライン引数をプログラム内で扱いやすくするために、Elixirでは`OptionParser`というモジュールを使用することができます。このモジュールを使うと、オプションフラグや引数の数を簡単にチェックすることができます。

##See Also
- [Elixir公式ドキュメント - System](https://hexdocs.pm/elixir/System.html)
- [Elixir公式ドキュメント - OptionParser](https://hexdocs.pm/elixir/OptionParser.html)