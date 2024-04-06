---
date: 2024-01-26 03:48:43.558016-07:00
description: "\u65B9\u6CD5\uFF1A Elixir\u306B\u306F`:debugger`\u3068\u3044\u3046\u30D3\
  \u30EB\u30C8\u30A4\u30F3\u306E\u30B0\u30E9\u30D5\u30A3\u30AB\u30EB\u30C7\u30D0\u30C3\
  \u30AC\u304C\u4ED8\u5C5E\u3057\u3066\u3044\u307E\u3059\u3002\u3053\u308C\u3092\u4F7F\
  \u7528\u3059\u308B\u306B\u306F\u3001\u305D\u308C\u3092\u8D77\u52D5\u3057\u3066\u5B9F\
  \u884C\u4E2D\u306E\u30D7\u30ED\u30BB\u30B9\u306B\u30A2\u30BF\u30C3\u30C1\u3059\u308B\
  \u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\u3002 \u307E\u305A\u3001`iex`\u30BB\u30C3\
  \u30B7\u30E7\u30F3\u5185\u3067`:debugger`\u304C\u8D77\u52D5\u3057\u3066\u3044\u308B\
  \u3053\u3068\u3092\u78BA\u8A8D\u3057\u3066\u304F\u3060\u3055\u3044\uFF1A."
lastmod: '2024-04-05T21:53:42.564656-06:00'
model: gpt-4-0125-preview
summary: "\u307E\u305A\u3001`iex`\u30BB\u30C3\u30B7\u30E7\u30F3\u5185\u3067`:debugger`\u304C\
  \u8D77\u52D5\u3057\u3066\u3044\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3057\u3066\u304F\
  \u3060\u3055\u3044\uFF1A."
title: "\u30C7\u30D0\u30C3\u30AC\u30FC\u306E\u4F7F\u3044\u65B9"
weight: 35
---

## 方法：
Elixirには`:debugger`というビルトインのグラフィカルデバッガが付属しています。これを使用するには、それを起動して実行中のプロセスにアタッチする必要があります。

まず、`iex`セッション内で`:debugger`が起動していることを確認してください：
```elixir
iex> :debugger.start()
{:ok, #PID<0.108.0>}
```

次に、デバッグしたいコードモジュールを解釈します：
```elixir
iex> :int.ni(MyApp.MyModule)
{:module, MyApp.MyModule}
```

ブレークポイントを設定できます：
```elixir
iex> :int.break(MyApp.MyModule, line_number)
:ok
```

そして、関数を実行してブレークポイントをヒットし、コードをステップ実行します：
```elixir
iex> MyApp.MyModule.my_function(arg1, arg2)
# デバッガはブレークポイントのある行で実行を一時停止します
```

## 深掘り
Elixirの`:debugger`の前に、Elixirが使用しているデバッガを提供したのはErlangです。これは頑丈であり、Erlang VM (BEAM)の強みである並行プロセスを扱うのに適しています。他の一部のデバッガとは異なり、`:debugger`はElixirのデータが不変であるため、flyで変数を変更することはできません。代替手段として、コードの任意のポイントで実行を一時停止してREPLにジャンプすることができる`IEx.pry`があり、これは非常に便利です。

`:debugger`はグラフィカルインターフェースに適していますが、コードをステップ実行することを特に目的としていないものの、プロセス検査とシステムメトリックスを提供するビルトインの`:observer`ツールを好む人もいるでしょう。Elixirのコミュニティも、デフォルトのデバッグツールを超えて`visualixir`や`rexbug`のようなツールを提供し、デバッグツールのエコシステムを拡大しています。

## 参照
- 公式Elixirデバッグに関する入門ガイド：https://elixir-lang.org/getting-started/debugging.html
- Erlangの`:debugger`ドキュメント：http://erlang.org/doc/apps/debugger/debugger_chapter.html
- デバッグ技術に関するElixir Forumの議論：https://elixirforum.com/c/elixir-questions/elixir-questions-questions-help/15
