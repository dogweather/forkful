---
title:                "コンピュータプログラミングにおける「コマンドライン引数の読み取り」"
html_title:           "Elixir: コンピュータプログラミングにおける「コマンドライン引数の読み取り」"
simple_title:         "コンピュータプログラミングにおける「コマンドライン引数の読み取り」"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 何 & なぜ？

コマンドライン引数を読み込むこととは、プログラマーがプログラムを実行する際に、そのプログラムに渡したい情報を指定することです。プログラマーは、プログラムの挙動を柔軟に制御するためにコマンドライン引数を使用します。

## 方法：

```Elixir
args = System.argv
IO.inspect args
```
実行すると、"Elixir my_program.exs arg1 arg2 arg3"のようなコマンドラインから渡されたすべての引数が表示されます。コマンドライン引数はリストとして格納され、プログラム内で使用することができます。

## 深く掘り下げる：

コマンドライン引数のアイデアは、コンピューター科学の歴史の中で長い間使用されてきました。エリクサー以外にも、PythonやJavaなどの他のプログラミング言語でも同様の引数読み込みの方法が使われています。実装の詳細については、Systemモジュールのドキュメントを参照することができます。

## 関連リンク：

- [ElixirのSystemモジュールのドキュメント](https://hexdocs.pm/elixir/System.html)
- [Pythonのコマンドライン引数チュートリアル](https://pymotw.com/3/argparse/)
- [Javaのコマンドライン引数チュートリアル](https://www.geeksforgeeks.org/command-line-arguments-in-java/)