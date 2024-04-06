---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:51.416443-07:00
description: "\u65B9\u6CD5\uFF1A \u30DE\u30C3\u30D7\u3092\u4F5C\u6210\u3059\u308B\u3053\
  \u3068\u306F\u7C21\u5358\u3067\u3059\u3002`%{}`\u69CB\u6587\u3092\u4F7F\u7528\u3057\
  \u307E\u3059\u3001\u4EE5\u4E0B\u306E\u3088\u3046\u306B\uFF1A."
lastmod: '2024-04-05T22:37:49.934975-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A \u30DE\u30C3\u30D7\u3092\u4F5C\u6210\u3059\u308B\u3053\
  \u3068\u306F\u7C21\u5358\u3067\u3059\u3002`%{}`\u69CB\u6587\u3092\u4F7F\u7528\u3057\
  \u307E\u3059\u3001\u4EE5\u4E0B\u306E\u3088\u3046\u306B\uFF1A."
title: "\u9023\u60F3\u914D\u5217\u306E\u4F7F\u7528"
weight: 15
---

## 方法：
マップを作成することは簡単です。`%{}`構文を使用します、以下のように：

```elixir
my_map = %{"name" => "Alex", "age" => 32}
IO.inspect(my_map)
```

値へのアクセスは、キーを使用して行われます：

```elixir
IO.puts my_map["name"]
```
出力：`Alex`

値を追加または更新するには、`Map.put/3` 関数を使用できます：

```elixir
updated_map = Map.put(my_map, "location", "NY")
IO.inspect(updated_map)
```
出力：`%{"age" => 32, "location" => "NY", "name" => "Alex"}`

キーを削除することは、`Map.delete/2`を使用して同じくらい簡単です：

```elixir
trimmed_map = Map.delete(updated_map, "age")
IO.inspect(trimmed_map)
```
出力：`%{"location" => "NY", "name" => "Alex"}`

## ディープダイブ
Elixirのマップは、RubyのハッシュやPythonの辞書など、古いキー・バリュー保存タイプの進化形です。これらは、より効率的な検索と挿入を可能にし、現代のElixirプログラミングにおいて行く先です。マップの前は、ElixirはHashDictとDictモジュールを使用していましたが、これらは今は非推奨です。

しかし、順序付けされたデータが必要なシナリオでは、Elixirのキーワードリストを検討するかもしれません。これらはタプルのリストで、小さなコレクションには効率的ですが、大きなデータセットにはマップほどパフォーマンスが良くありません。

マップはキーを「フラット」な構造で保存するため、ネストされた値への直接アクセスは少し厄介です。深いネスティングについては、より動的なネストされたデータ操作に向けて、`get_in`、`put_in`、`update_in`、及び`get_and_update_in` 関数を通じて構造化されたアクセスを検討するかもしれません。

要するに、マップはElixirでの連想配列ニーズに対するあなたの最適な選択肢ですが、言語は各シナリオに対する豊富なデータ構造を提供しており、ジョブに適した正しいツールを選ぶように促しています。
