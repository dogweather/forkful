---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:22.310217-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.638962-06:00'
model: gpt-4-0125-preview
summary: "JSON\u3092\u6271\u3046\u3053\u3068\u306B\u306F\u3001JSON\u5F62\u5F0F\u306E\
  \u6587\u5B57\u5217\u3092Elixir\u304C\u64CD\u4F5C\u3067\u304D\u308B\u30C7\u30FC\u30BF\
  \u69CB\u9020\u306B\u89E3\u6790\u3057\u3001Elixir\u306E\u30C7\u30FC\u30BF\u69CB\u9020\
  \u3092JSON\u6587\u5B57\u5217\u306B\u30B7\u30EA\u30A2\u30E9\u30A4\u30BA\uFF08\u76F4\
  \u5217\u5316\uFF09\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u304C\u542B\u307E\u308C\u307E\
  \u3059\u3002\u3053\u308C\u306F\u3001JSON\u304C\u8EFD\u91CF\u3067\u3001\u30C6\u30AD\
  \u30B9\u30C8\u30D9\u30FC\u30B9\u3001\u8A00\u8A9E\u975E\u4F9D\u5B58\u306E\u30C7\u30FC\
  \u30BF\u4EA4\u63DB\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3068\u3057\u3066\u5E83\u304F\
  \u4F7F\u308F\u308C\u308B\u305F\u3081\u3001\u30A6\u30A7\u30D6\u958B\u767A\u3001API\u3001\
  \u8A2D\u5B9A\u30D5\u30A1\u30A4\u30EB\u3067\u6B20\u304B\u305B\u306A\u3044\u3082\u306E\
  \u3067\u3059\u3002\u305D\u306E\u5358\u7D14\u3055\u3068\u4EBA\u9593\u304C\u8AAD\u307F\
  \u3084\u3059\u3044\u5F62\u5F0F\u306E\u305F\u3081\u306B\u5E83\u304F\u4F7F\u7528\u3055\
  \u308C\u3066\u3044\u307E\u3059\u3002."
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
