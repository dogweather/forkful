---
date: 2024-01-26 03:48:43.558016-07:00
description: "Elixir\u3067\u30C7\u30D0\u30C3\u30AC\u3092\u4F7F\u7528\u3059\u308B\u3068\
  \u306F\u3001\u30B3\u30FC\u30C9\u3092\u30B9\u30C6\u30C3\u30D7\u5B9F\u884C\u3057\u3066\
  \u3001\u5909\u6570\u3092\u8ABF\u67FB\u3057\u3001\u30D5\u30ED\u30FC\u3092\u8FFD\u8DE1\
  \u3057\u3066\u30D0\u30B0\u3092\u6291\u5236\u3059\u308B\u3053\u3068\u3092\u610F\u5473\
  \u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u4E88\u671F\
  \u305B\u306C\u6319\u52D5\u3092\u7406\u89E3\u3057\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\
  \u30B7\u30E7\u30F3\u304C\u8A2D\u8A08\u3069\u304A\u308A\u306B\u52D5\u4F5C\u3057\u3066\
  \u3044\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3059\u308B\u305F\u3081\u306B\u3053\u308C\
  \u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: 2024-02-19 22:05:00.894591
model: gpt-4-0125-preview
summary: "Elixir\u3067\u30C7\u30D0\u30C3\u30AC\u3092\u4F7F\u7528\u3059\u308B\u3068\
  \u306F\u3001\u30B3\u30FC\u30C9\u3092\u30B9\u30C6\u30C3\u30D7\u5B9F\u884C\u3057\u3066\
  \u3001\u5909\u6570\u3092\u8ABF\u67FB\u3057\u3001\u30D5\u30ED\u30FC\u3092\u8FFD\u8DE1\
  \u3057\u3066\u30D0\u30B0\u3092\u6291\u5236\u3059\u308B\u3053\u3068\u3092\u610F\u5473\
  \u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u4E88\u671F\
  \u305B\u306C\u6319\u52D5\u3092\u7406\u89E3\u3057\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\
  \u30B7\u30E7\u30F3\u304C\u8A2D\u8A08\u3069\u304A\u308A\u306B\u52D5\u4F5C\u3057\u3066\
  \u3044\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3059\u308B\u305F\u3081\u306B\u3053\u308C\
  \u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u30C7\u30D0\u30C3\u30AC\u30FC\u306E\u4F7F\u3044\u65B9"
---

{{< edit_this_page >}}

## 何となぜ？
Elixirでデバッガを使用するとは、コードをステップ実行して、変数を調査し、フローを追跡してバグを抑制することを意味します。プログラマーは、予期せぬ挙動を理解し、アプリケーションが設計どおりに動作していることを確認するためにこれを行います。

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
