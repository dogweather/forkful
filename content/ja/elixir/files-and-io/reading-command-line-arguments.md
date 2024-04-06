---
date: 2024-01-20 17:55:42.741846-07:00
description: "How to (\u65B9\u6CD5) Elixir\u3067\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\
  \u30F3\u5F15\u6570\u3092\u8AAD\u3080\u306E\u306F\u7C21\u5358\u3060\u3002Erlang VM\u4E0A\
  \u3067\u52D5\u304FElixir\u306F\u3001\u53E4\u304F\u304B\u3089\u9032\u5316\u3057\u3066\
  \u304D\u305FErlang\u306E\u8C4A\u5BCC\u306A\u6A5F\u80FD\u3092\u6D3B\u7528\u3067\u304D\
  \u308B\u3002\u4ED6\u306E\u8A00\u8A9E\u3067\u306F\u5F15\u6570\u89E3\u6790\u306E\u305F\
  \u3081\u306B\u5916\u90E8\u30E9\u30A4\u30D6\u30E9\u30EA\u304C\u5FC5\u8981\u304B\u3082\
  \u3057\u308C\u306A\u3044\u304C\u3001Elixir\u306F`System.argv()`\u304C\u305D\u306E\
  \u307E\u307E\u4F7F\u3048\u308B\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.577137-06:00'
model: gpt-4-1106-preview
summary: "\u3057\u304B\u3057\u3001\u3088\u308A\u8907\u96D1\u306A\u5F15\u6570\u89E3\
  \u6790\u304C\u5FC5\u8981\u306A\u6642\u306F\u3001`OptionParser`\u30E2\u30B8\u30E5\
  \u30FC\u30EB\u304C\u4F7F\u3048\u308B\u3002\u3053\u308C\u306B\u3088\u308A\u3001\u30D5\
  \u30E9\u30B0\u3084\u30AD\u30FC/\u5024\u30AA\u30D7\u30B7\u30E7\u30F3\u306A\u3069\u3092\
  \u6271\u3048\u308B\u3088\u3046\u306B\u306A\u308B\u3002\u3069\u3061\u3089\u306E\u65B9\
  \u6CD5\u3082\u3001\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u30C4\u30FC\u30EB\u3092\
  \u4F5C\u6210\u3059\u308B\u305F\u3081\u306B\u5B9F\u88C5\u304C\u30B7\u30F3\u30D7\u30EB\
  \u3067\u4FE1\u983C\u6027\u304C\u9AD8\u3044\u3002"
title: "\u30B3\u30DE\u30F3\u30C9\u30E9\u30A4\u30F3\u5F15\u6570\u306E\u8AAD\u307F\u53D6\
  \u308A"
weight: 23
---

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
