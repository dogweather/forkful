---
date: 2024-01-20 17:52:14.252444-07:00
description: "How to: (\u3084\u308A\u65B9) Elixir\u3067\u306F\u3001`IO.inspect/2`\u3068\
  `IO.puts/1`\u3092\u3088\u304F\u4F7F\u3044\u307E\u3059\u3002\u3053\u308C\u3089\u306F\
  \u30BF\u30FC\u30DF\u30CA\u30EB\u306B\u60C5\u5831\u3092\u51FA\u529B\u3057\u307E\u3059\
  \u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.615540-06:00'
model: gpt-4-1106-preview
summary: "Elixir\u3067\u306F\u3001`IO.inspect/2`\u3068`IO.puts/1`\u3092\u3088\u304F\
  \u4F7F\u3044\u307E\u3059\u3002\u3053\u308C\u3089\u306F\u30BF\u30FC\u30DF\u30CA\u30EB\
  \u306B\u60C5\u5831\u3092\u51FA\u529B\u3057\u307E\u3059."
title: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3092\u8868\u793A\u3059\u308B"
weight: 33
---

## How to: (やり方)
Elixirでは、`IO.inspect/2`と`IO.puts/1`をよく使います。これらはターミナルに情報を出力します。

```elixir
# 値を出力して変数をそのまま返します。
value = "Hello, World!"
IO.inspect(value)
# Output: "Hello, World!"

# 文字列を出力しますが、返り値は :ok となります。
IO.puts("Debugging my program")
# Output: Debugging my program
```

## Deep Dive (深堀り)
Elixirの前身であるErlangは、1987年に開発されました。開発の初期から、ElixirやErlangでは関数の戻り値としてデバッグ情報を出力することが一般的でした。`Logger`モジュールなどの代替手段もあります。これにより、設定に基づいてログレベルを柔軟に制御できます。

```elixir
# Loggerを使用して情報を出力する
require Logger

Logger.debug("Debug info: #{inspect(value)}")
```

他の言語とは異なり、Elixirの`IO.inspect/2`はデバッグ出力を行いつつ、値を変更せずに返す点が特徴です。つまり、デバッグ行を削除せずにコード内に残しても、プログラムの挙動に影響を与えません。

## See Also (参考)
- ElixirのIOモジュールのドキュメント: [https://hexdocs.pm/elixir/IO.html](https://hexdocs.pm/elixir/IO.html)
- ElixirのLoggerモジュールのドキュメント: [https://hexdocs.pm/logger/Logger.html](https://hexdocs.pm/logger/Logger.html)
