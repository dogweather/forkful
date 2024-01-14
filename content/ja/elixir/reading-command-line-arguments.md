---
title:    "Elixir: コンピュータプログラミングの記事のタイトル：コマンドライン引数の読み取り"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## なぜ

コマンドライン引数を読むことの *なぜ* は、Elixir プログラミングにおいて非常に重要なスキルです。プログラムを実行する際に、ユーザーの入力を受け取らなければならない場合があります。そのような場合に、コマンドライン引数を読むことで、ユーザーは実行時に動的にプログラムの挙動を変更することができます。

## 使い方

コマンドライン引数を読むには、`System.argv`関数を使用します。この関数は、文字列のリストを返し、それぞれがユーザーが入力した引数に対応しています。例えば、下記のようなコマンドを実行すると...

```
elixir example.exs apple banana cherry
```

...次のようなコードを使用することで、ユーザーが入力した引数を取得することができます。

```elixir
args = System.argv

IO.puts "最初の引数: #{List.first(args)}"
IO.puts "二番目の引数: #{List.at(args, 1)}"
IO.puts "三番目の引数: #{List.at(args, 2)}"
```

このコードの出力は次のようになります。

```
最初の引数: apple
二番目の引数: banana
三番目の引数: cherry
```

## ディープダイブ

コマンドライン引数をより詳しく調べると、`System.argv`がどのように動作するのかがわかります。この関数は、Elixirの特殊な変数である`ARGV`を使用して、引数を取得します。`ARGV`は、システムがプログラムに渡した引数をリストとして保持するための変数です。

`System.argv`は、`ARGV`のコピーを返すため、変数の値を変更することなく、引数を読むことができます。

## 参考リンク

* [Elixir 公式ドキュメント (日本語版)](https://hexdocs.pm/elixir/)
* [コマンドライン引数を5分で学ぶ](https://medium.com/@taka0x2a/learn-command-line-arguments-in-5-minutes-e9ed2c9e6846)
* [Elixir の特殊な変数としての `ARGV`](https://elixirschool.com/jp/lessons/basics/basics/#elixir-%E3%81%AE%E7%89%B9%E6%AE%8A%E3%81%AA%E5%A4%89%E6%95%B0%E3%81%A8%E3%81%97%E3%81%A6%E3%81%AE-argv)

## 参考

この記事では、Elixir でのコマンドライン引数の読み方について説明しました。`System.argv`関数を使用することで、実行時にユーザーが入力した引数をプログラム内で利用することができます。この機能を使用することで、より柔軟なプログラムを作成することができます。

どうぞご活用ください！