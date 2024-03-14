---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:51.416443-07:00
description: "Elixir\u3067\u306F\u3001\u30DE\u30C3\u30D7\u3068\u547C\u3070\u308C\u308B\
  \u9023\u60F3\u914D\u5217\u306F\u3001\u4E00\u610F\u306E\u30AD\u30FC\u304C\u5024\u3092\
  \u6307\u3057\u793A\u3059\u30AD\u30FC\u30FB\u30D0\u30EA\u30E5\u30FC\u30DA\u30A2\u306E\
  \u30B3\u30EC\u30AF\u30B7\u30E7\u30F3\u3067\u3059\u3002\u30C7\u30FC\u30BF\u306E\u4FDD\
  \u5B58\u3084\u53D6\u5F97\u306B\u975E\u5E38\u306B\u4FBF\u5229\u3067\u3001\u30B3\u30FC\
  \u30C9\u3092\u30AF\u30EA\u30FC\u30CA\u30FC\u306B\u3057\u3001\u3042\u306A\u305F\u306E\
  \u751F\u6D3B\u3092\u697D\u306B\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.602738-06:00'
model: gpt-4-0125-preview
summary: "Elixir\u3067\u306F\u3001\u30DE\u30C3\u30D7\u3068\u547C\u3070\u308C\u308B\
  \u9023\u60F3\u914D\u5217\u306F\u3001\u4E00\u610F\u306E\u30AD\u30FC\u304C\u5024\u3092\
  \u6307\u3057\u793A\u3059\u30AD\u30FC\u30FB\u30D0\u30EA\u30E5\u30FC\u30DA\u30A2\u306E\
  \u30B3\u30EC\u30AF\u30B7\u30E7\u30F3\u3067\u3059\u3002\u30C7\u30FC\u30BF\u306E\u4FDD\
  \u5B58\u3084\u53D6\u5F97\u306B\u975E\u5E38\u306B\u4FBF\u5229\u3067\u3001\u30B3\u30FC\
  \u30C9\u3092\u30AF\u30EA\u30FC\u30CA\u30FC\u306B\u3057\u3001\u3042\u306A\u305F\u306E\
  \u751F\u6D3B\u3092\u697D\u306B\u3057\u307E\u3059\u3002"
title: "\u9023\u60F3\u914D\u5217\u306E\u4F7F\u7528"
---

{{< edit_this_page >}}

## 何となぜ？

Elixirでは、マップと呼ばれる連想配列は、一意のキーが値を指し示すキー・バリューペアのコレクションです。データの保存や取得に非常に便利で、コードをクリーナーにし、あなたの生活を楽にします。

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
