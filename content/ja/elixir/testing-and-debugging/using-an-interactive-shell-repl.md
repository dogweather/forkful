---
date: 2024-01-26 04:13:12.301743-07:00
description: "\u65B9\u6CD5\uFF1A IEx\u3092\u8D77\u52D5\u3059\u308B\u306B\u306F\u3001\
  \u30BF\u30FC\u30DF\u30CA\u30EB\u3092\u958B\u3044\u3066`iex`\u3068\u5165\u529B\u3057\
  \u307E\u3059\u3002\u4EE5\u4E0B\u306B\u305D\u306E\u4E00\u4F8B\u3092\u793A\u3057\u307E\
  \u3059\uFF1A."
lastmod: '2024-03-13T22:44:41.614365-06:00'
model: gpt-4-0125-preview
summary: "IEx\u3092\u8D77\u52D5\u3059\u308B\u306B\u306F\u3001\u30BF\u30FC\u30DF\u30CA\
  \u30EB\u3092\u958B\u3044\u3066`iex`\u3068\u5165\u529B\u3057\u307E\u3059\u3002\u4EE5\
  \u4E0B\u306B\u305D\u306E\u4E00\u4F8B\u3092\u793A\u3057\u307E\u3059\uFF1A."
title: "\u30A4\u30F3\u30BF\u30E9\u30AF\u30C6\u30A3\u30D6\u30B7\u30A7\u30EB\uFF08REPL\uFF09\
  \u306E\u4F7F\u7528"
weight: 34
---

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
