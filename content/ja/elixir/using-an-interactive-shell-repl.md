---
title:                "インタラクティブシェル（REPL）の使用"
date:                  2024-01-26T04:13:12.301743-07:00
model:                 gpt-4-0125-preview
simple_title:         "インタラクティブシェル（REPL）の使用"

category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## 何となぜ？
インタラクティブシェル、またはREPL（Read-Eval-Print Loop）を利用すると、リアルタイムでコードスニペットを試すことができます。Elixirプログラマーは、IEx（Interactive Elixir）と呼ばれるREPLを実験、デバッグ、そして言語学習のために使用します。

## 方法：
IExを起動するには、ターミナルを開いて`iex`と入力します。以下にその一例を示します：

```Elixir
iex> name = "Elixir Programmer"
"Elixir Programmer"
iex> String.length(name)
17
iex> Enum.map([1, 2, 3], fn num -> num * 3 end)
[3, 6, 9]
```

出力には変数割り当て、関数結果、そして無名関数の動作が表示されるはずです。

## 深掘り
IExシェルは、Elixirの初期からその一部として存在しています。Elixirの創造者であるJosé Valimは、Pythonの`python`やRubyの`irb`など、他の言語のインタラクティブシェルからインスピレーションを得ました。IExはこれらと多くの機能を共有していますが、Elixirの並行性を扱うために設計されており、Erlang VMの機能と完全に統合されています。

Erlangエコシステム内のIExの代替品には`erl`、Erlangシェルがあります。しかし、IExは包括的なタブ補完、履歴、ヘルパーなど、よりElixirフレンドリーな環境を提供します。

IEx REPLは遊び場以上のものです；実行中のシステムにシームレスに接続することができます。これはライブアプリケーションのデバッグにとって重要です。その基礎となる実装はBEAM（Erlang VM）に依存しており、シェル内でのホットコードスワッピングなどの機能がサポートされています。

## 参照
さらなる読み物とリソースについては、こちらをチェックしてください：

- [ElixirのIExドキュメント](https://hexdocs.pm/iex/IEx.html)
- [Interactive Elixir (IEx) - The Elixir Shell](https://elixir-lang.org/getting-started/introduction.html#interactive-elixir)
- [Erlangの`erl`ドキュメント](http://erlang.org/doc/man/erl.html)
- [Elixirのインタラクティブシェルを学ぶ](https://elixirschool.com/en/lessons/basics/iex_helpers/)
