---
title:                "デバッグ出力を表示する"
aliases:
- /ja/elixir/printing-debug-output/
date:                  2024-01-20T17:52:14.252444-07:00
model:                 gpt-4-1106-preview
simple_title:         "デバッグ出力を表示する"

tag:                  "Testing and Debugging"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

プログラムで何が起こっているかを理解するため、デバッグ出力を使います。これは、問題の解決やコードの理解を助けるために行われます。

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
