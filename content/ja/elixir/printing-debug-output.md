---
title:    "Elixir: デバッグ出力の印刷"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## なぜ
Elixirでのデバッグ出力を表示するのか、その理由をご存知ですか？デバッグ出力を使用することで、プログラムの実行中に各ステップを確認することができます。これにより、コードの動作を理解し、エラーを特定することができます。

## 方法
デバッグ出力を表示するには、コードの中に`IO.inspect/2`を使用します。以下のような構文を使用してください。
```Elixir
IO.inspect(value, label: "デバッグ出力")
```
このようにすると、`value`の値がラベルとともに出力されます。例えば、`age = 25`というコードがあった場合、以下のように表示されます。
```
デバッグ出力: 25
```
また、モジュール内のすべての関数にデバッグ出力を追加する場合は、`using`キーワードを使用して、自動的に`IO.inspect`を挿入することができます。以下のように使用します。
```Elixir
defmodule Example do
  using :debug

  def add(a, b) do
    a + b
  end

  def multiply(a, b) do
    a * b
  end
end
```
これにより、関数の実行時に自動的にデバッグ出力が表示されます。

## 深堀り
デバッグ出力を表示する際には、`IO.inspect`にいくつかのオプションを追加することもできます。例えば、`depth`オプションを使用すると、ネストされたデータ構造内の階層を制限できます。以下のように使用します。
```Elixir
IO.inspect(data, depth: 2)
```
また、`color: [:cyan, :bg_black]`オプションを使用すると、ターミナル上でデバッグ出力をカラー表示することもできます。

## See Also
- [ElixirのIOモジュール](https://hexdocs.pm/elixir/IO.html)
- [プロダクション環境でのデバッグ出力の制御](https://elixir-lang.org/getting-started/debugging.html#controlling-debug-output-in-production-environments)