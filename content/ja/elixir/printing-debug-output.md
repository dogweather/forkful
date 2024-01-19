---
title:                "デバッグ出力の印刷"
html_title:           "Fish Shell: デバッグ出力の印刷"
simple_title:         "デバッグ出力の印刷"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

# Elixirにおけるデバッグ出力の表示: 説明から技術まで

## 何となぜ?
デバッグ出力とは、プログラムの動作を理解しやすくするためにコンソールに表示される情報のことです。これにより、プログラマーはツールやアプリケーションの動作確認が行いやすくなります。

## どうするか:
Elixirでは`IO.inspect/2`関数を使用してデバッグ出力を簡単に行うことができます。次のように使用します。

```elixir
defmodule DebugExample do
  def run do
    [1, 2, 3]
    |> IO.inspect(label: "before")
    |> Enum.map(&(&1 * 2))
    |> IO.inspect(label: "after")
  end
end

DebugExample.run
```

このコードの出力は次のようになります。

```console
before: [1, 2, 3]
after: [2, 4, 6]
```

これは非常に簡単にデバッグ情報を表示してくれます。

## 深堀り:
元々、Elixirのツールとしては`IO.inspect/2`は存在していませんでした。しかしプログラマーたちはより効率的なデバッグのために、これを追加することを決定しました。これはElixirが問題解決に必要なツールを積極的に提供している歴史的背景を反映しています。

代わりとして、通常の`IO.puts/2`でもデバッグ情報を表示することは可能です。ただし、この方法ではデータの詳細な表示やラベル付けはできません。詳細なデバッグ出力が必要な場合は、`IO.inspect/2`の使用が推奨されます。

`IO.inspect/2`関数は、内部で`Inspect`モジュールの機能を利用していることに注意してください。このモジュールは任意のデータ型を人間が読める文字列に変換する責任があります。

## 参考文献:
- [IO モジュールの公式ドキュメント](https://hexdocs.pm/elixir/IO.html)
- [ElixirのInspectモジュールについてのドキュメンテーション](https://hexdocs.pm/elixir/Inspect.html)