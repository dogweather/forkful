---
date: 2024-01-20 17:55:42.741846-07:00
description: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u3080\
  \u3053\u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u5B9F\u884C\u6642\u306B\u8FFD\
  \u52A0\u60C5\u5831\u3092\u6E21\u3059\u65B9\u6CD5\u3060\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u306F\u72B6\u6CC1\u306B\u5FDC\u3058\u3066\u30AB\u30B9\u30BF\u30DE\u30A4\u30BA\
  \u3057\u305F\u632F\u308B\u821E\u3044\u3092\u30D7\u30ED\u30B0\u30E9\u30E0\u306B\u3055\
  \u305B\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u5229\u7528\u3059\u308B\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.631748-06:00'
model: gpt-4-1106-preview
summary: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u3092\u8AAD\u3080\
  \u3053\u3068\u306F\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u5B9F\u884C\u6642\u306B\u8FFD\
  \u52A0\u60C5\u5831\u3092\u6E21\u3059\u65B9\u6CD5\u3060\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u306F\u72B6\u6CC1\u306B\u5FDC\u3058\u3066\u30AB\u30B9\u30BF\u30DE\u30A4\u30BA\
  \u3057\u305F\u632F\u308B\u821E\u3044\u3092\u30D7\u30ED\u30B0\u30E9\u30E0\u306B\u3055\
  \u305B\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u5229\u7528\u3059\u308B\u3002"
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

コマンドライン引数を読むことは、プログラム実行時に追加情報を渡す方法だ。プログラマは状況に応じてカスタマイズした振る舞いをプログラムにさせるためにこれを利用する。

## How to (方法)

```elixir
# コマンドライン引数を取得するには
args = System.argv()
# 例を見てみよう
IO.inspect(args)

# 実行時はこのようにする:
# elixir my_script.exs arg1 arg2 arg3
```

```elixir
# 引数に応じて異なるアクションを取る
defmodule CLIExample do
  def main(args) do
    case args do
      ["hello"] -> IO.puts "Hello, World!"
      [name] when is_binary(name) -> IO.puts "Hello, #{name}!"
      _ -> IO.puts "Usage: command [name]"
    end
  end
end

CLIExample.main(System.argv())

# 実行例:
# elixir my_script.exs hello
# Hello, World!
# elixir my_script.exs Taro
# Hello, Taro!
# elixir my_script.exs
# Usage: command [name]
```

## Deep Dive (深掘り)

Elixirでコマンドライン引数を読むのは簡単だ。Erlang VM上で動くElixirは、古くから進化してきたErlangの豊富な機能を活用できる。他の言語では引数解析のために外部ライブラリが必要かもしれないが、Elixirは`System.argv()`がそのまま使える。

しかし、より複雑な引数解析が必要な時は、`OptionParser`モジュールが使える。これにより、フラグやキー/値オプションなどを扱えるようになる。どちらの方法も、コマンドラインツールを作成するために実装がシンプルで信頼性が高い。

## See Also (関連情報)

- [Elixirの公式ドキュメント](https://elixir-lang.org/docs.html)
- [OptionParserモジュールのドキュメント](https://hexdocs.pm/elixir/OptionParser.html)
