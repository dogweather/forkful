---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:22.310217-07:00
description: "\u65B9\u6CD5\uFF1A Elixir\u3067\u306F\u3001`Jason`\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u3092\u4F7F\u7528\u3057\u3066JSON\u306E\u89E3\u6790\u3068\u751F\u6210\
  \u3092\u884C\u3046\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002\u3053\u308C\u306F\
  JSON\u89E3\u6790\u3068\u751F\u6210\u306B\u304A\u3044\u3066\u4EBA\u6C17\u306E\u9078\
  \u629E\u80A2\u3067\u3059\u3002\u307E\u305A\u3001`mix.exs`\u306B`Jason`\u3092\u30D7\
  \u30ED\u30B8\u30A7\u30AF\u30C8\u306E\u4F9D\u5B58\u95A2\u4FC2\u306B\u8FFD\u52A0\u3057\
  \u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:41.638962-06:00'
model: gpt-4-0125-preview
summary: "Elixir\u3067\u306F\u3001`Jason`\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4F7F\
  \u7528\u3057\u3066JSON\u306E\u89E3\u6790\u3068\u751F\u6210\u3092\u884C\u3046\u3053\
  \u3068\u304C\u3067\u304D\u307E\u3059\u3002\u3053\u308C\u306FJSON\u89E3\u6790\u3068\
  \u751F\u6210\u306B\u304A\u3044\u3066\u4EBA\u6C17\u306E\u9078\u629E\u80A2\u3067\u3059\
  \u3002\u307E\u305A\u3001`mix.exs`\u306B`Jason`\u3092\u30D7\u30ED\u30B8\u30A7\u30AF\
  \u30C8\u306E\u4F9D\u5B58\u95A2\u4FC2\u306B\u8FFD\u52A0\u3057\u307E\u3059\uFF1A."
title: "JSON\u3092\u6D3B\u7528\u3059\u308B"
weight: 38
---

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
