---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:22.310217-07:00
description: "\u2026"
lastmod: 2024-02-19 22:05:00.915638
model: gpt-4-0125-preview
summary: "\u2026"
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？

JSONを扱うことには、JSON形式の文字列をElixirが操作できるデータ構造に解析し、Elixirのデータ構造をJSON文字列にシリアライズ（直列化）するプロセスが含まれます。これは、JSONが軽量で、テキストベース、言語非依存のデータ交換フォーマットとして広く使われるため、ウェブ開発、API、設定ファイルで欠かせないものです。その単純さと人間が読みやすい形式のために広く使用されています。

## 方法：

Elixirでは、`Jason`ライブラリを使用してJSONの解析と生成を行うことができます。これはJSON解析と生成において人気の選択肢です。まず、`mix.exs`に`Jason`をプロジェクトの依存関係に追加します：

```elixir
defp deps do
  [
    {:jason, "~> 1.3"}
  ]
end
```

次に、`mix deps.get`を実行して依存関係を取得します。

### JSONの解析：
JSON文字列をElixirのデータ構造に変換するには：

```elixir
json_string = "{\"name\":\"John\", \"age\":30}"
{:ok, person} = Jason.decode(json_string)
IO.inspect(person)
# 出力: %{"name" => "John", "age" => 30}
```

### JSONの生成：
ElixirのマップをJSON文字列に変換するには：

```elixir
person_map = %{"name" => "Jane", "age" => 25}
{:ok, json_string} = Jason.encode(person_map)
IO.puts(json_string)
# 出力: {"age":25,"name":"Jane"}
```

### 構造体を扱う：
Elixirの構造体をエンコードするには、構造体に対して`Jason.Encoder`プロトコルを実装する必要があります。こちらが一例です：

```elixir
defmodule Person do
  @derive {Jason.Encoder, only: [:name, :age]}
  defstruct name: nil, age: nil
end

person_struct = %Person{name: "Mike", age: 28}
{:ok, json_string} = Jason.encode(person_struct)
IO.puts(json_string)
# 出力: {"age":28,"name":"Mike"}
```

このシンプルなアプローチで、ElixirアプリケーションへのJSON処理の統合を始めることができ、様々なプログラミング環境でのデータ交換を促進します。
