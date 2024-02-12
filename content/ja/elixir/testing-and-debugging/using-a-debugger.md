---
title:                "デバッガーの使い方"
aliases:
- /ja/elixir/using-a-debugger/
date:                  2024-01-26T03:48:43.558016-07:00
model:                 gpt-4-0125-preview
simple_title:         "デバッガーの使い方"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/using-a-debugger.md"
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
