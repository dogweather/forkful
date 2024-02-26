---
date: 2024-01-26 04:13:12.301743-07:00
description: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\u3001\
  \u307E\u305F\u306FREPL\uFF08Read-Eval-Print Loop\uFF09\u3092\u5229\u7528\u3059\u308B\
  \u3068\u3001\u30EA\u30A2\u30EB\u30BF\u30A4\u30E0\u3067\u30B3\u30FC\u30C9\u30B9\u30CB\
  \u30DA\u30C3\u30C8\u3092\u8A66\u3059\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\
  Elixir\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001IEx\uFF08Interactive Elixir\uFF09\
  \u3068\u547C\u3070\u308C\u308BREPL\u3092\u5B9F\u9A13\u3001\u30C7\u30D0\u30C3\u30B0\
  \u3001\u305D\u3057\u3066\u8A00\u8A9E\u5B66\u7FD2\u306E\u305F\u3081\u306B\u4F7F\u7528\
  \u3057\u307E\u3059\u3002"
lastmod: '2024-02-25T18:49:39.767791-07:00'
model: gpt-4-0125-preview
summary: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\u3001\
  \u307E\u305F\u306FREPL\uFF08Read-Eval-Print Loop\uFF09\u3092\u5229\u7528\u3059\u308B\
  \u3068\u3001\u30EA\u30A2\u30EB\u30BF\u30A4\u30E0\u3067\u30B3\u30FC\u30C9\u30B9\u30CB\
  \u30DA\u30C3\u30C8\u3092\u8A66\u3059\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\
  Elixir\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001IEx\uFF08Interactive Elixir\uFF09\
  \u3068\u547C\u3070\u308C\u308BREPL\u3092\u5B9F\u9A13\u3001\u30C7\u30D0\u30C3\u30B0\
  \u3001\u305D\u3057\u3066\u8A00\u8A9E\u5B66\u7FD2\u306E\u305F\u3081\u306B\u4F7F\u7528\
  \u3057\u307E\u3059\u3002"
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
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
