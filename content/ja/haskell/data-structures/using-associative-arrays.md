---
changelog:
- 2024-01-30, dogweather, reviewed
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:56.507169-07:00
description: "Haskell\u306B\u304A\u3051\u308B\u9023\u60F3\u914D\u5217\u3001\u307E\u305F\
  \u306F\u8F9E\u66F8\u306F\u3001\u30AD\u30FC\u3068\u5024\u3092\u5BFE\u5FDC\u4ED8\u3051\
  \u3066\u7D20\u65E9\u3044\u691C\u7D22\u3068\u52B9\u7387\u7684\u306A\u30C7\u30FC\u30BF\
  \u7BA1\u7406\u3092\u5B9F\u73FE\u3059\u308B\u305F\u3081\u306E\u3082\u306E\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u8981\u7D20\u3092\u63A2\u3059\
  \u306E\u304C\u30EA\u30B9\u30C8\u3068\u6BD4\u8F03\u3057\u3066\u5BB9\u6613\u306A\u3001\
  \u30DA\u30A2\u30EA\u30F3\u30B0\u3055\u308C\u305F\u8981\u7D20\u306E\u96C6\u5408\u3092\
  \u6271\u3046\u305F\u3081\u306B\u3053\u308C\u3089\u3092\u4F7F\u7528\u3057\u307E\u3059\
  \u3002"
lastmod: '2024-03-13T22:44:42.173789-06:00'
model: gpt-4-0125-preview
summary: "Haskell\u306B\u304A\u3051\u308B\u9023\u60F3\u914D\u5217\u3001\u307E\u305F\
  \u306F\u8F9E\u66F8\u306F\u3001\u30AD\u30FC\u3068\u5024\u3092\u5BFE\u5FDC\u4ED8\u3051\
  \u3066\u7D20\u65E9\u3044\u691C\u7D22\u3068\u52B9\u7387\u7684\u306A\u30C7\u30FC\u30BF\
  \u7BA1\u7406\u3092\u5B9F\u73FE\u3059\u308B\u305F\u3081\u306E\u3082\u306E\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u8981\u7D20\u3092\u63A2\u3059\
  \u306E\u304C\u30EA\u30B9\u30C8\u3068\u6BD4\u8F03\u3057\u3066\u5BB9\u6613\u306A\u3001\
  \u30DA\u30A2\u30EA\u30F3\u30B0\u3055\u308C\u305F\u8981\u7D20\u306E\u96C6\u5408\u3092\
  \u6271\u3046\u305F\u3081\u306B\u3053\u308C\u3089\u3092\u4F7F\u7528\u3057\u307E\u3059\
  \u3002"
title: "\u9023\u60F3\u914D\u5217\u306E\u4F7F\u7528"
weight: 15
---

## 何となぜ？

Haskellにおける連想配列、または辞書は、キーと値を対応付けて素早い検索と効率的なデータ管理を実現するためのものです。プログラマーは、要素を探すのがリストと比較して容易な、ペアリングされた要素の集合を扱うためにこれらを使用します。

## どのようにして：

Haskellには、他の言語のようにそのまま連想配列が組み込まれているわけではありませんが、キーと値のペアを扱うための強力な標準ライブラリである`Data.Map`を提供しています。さあ、袖をまくって使い方を見てみましょう!

まず、インポートすることを確認してください：
```Haskell
import qualified Data.Map as Map
```

マップの作成は簡単です。いくつかのプログラミング言語とそれらのパラダイムを持つマップを作成してみましょう：
```Haskell
let languages = Map.fromList [("Haskell", "Functional"), ("Python", "Imperative"), ("Prolog", "Logical")]
```

さて、Haskellのパラダイムを取得してみましょうか？
```Haskell
Map.lookup "Haskell" languages
-- 出力: Just "Functional"
```

新しい言語を追加するのは簡単です：
```Haskell
let languagesUpdated = Map.insert "Rust" "Systems" languages
```

すべての言語をリストしたい場合は？ `Map.keys`を使ってください：
```Haskell
Map.keys languagesUpdated
-- 出力: ["Haskell","Python","Prolog","Rust"]
```

パラダイムをリストするには、`Map.elems`を使ってください：
```Haskell
Map.elems languagesUpdated
-- 出力: ["Functional","Imperative","Logical","Systems"]
```

これらの基本的な操作はほとんどの用途をカバーするはずですが、`Data.Map`でさらに多くのことを探求する余地はたくさんあります！

## ディープダイブ

Haskellの標準ライブラリにある`Data.Map`モジュールは、バランスのとれた二分木、特にAVL木の上に構築されています。この選択は、マップのほとんどの操作（挿入、削除、検索など）をO(log n)の時間で行えるようにするためであり、nはマップ内の要素の数です。これは多くの使用例に対して効率的な選択ですが、すべてのシナリオで最速ではありません。

歴史的なニュアンスもあります：`Data.Map`が主流になる前は、Haskellプログラマはしばしば連想配列をシミュレートするためにペアのリストを使用していました。しかしながら、そのような構造における操作は検索についてO(n)であり、パフォーマンスの観点から`Data.Map`はかなりの改善をもたらしました。

それにもかかわらず、`Data.Map`が効率と利便性を兼ね備えているにもかかわらず、すべての仕事に最適なツールであるわけではありません。O(log n)の検索時間が遅すぎるか、キーが常に整数値であるような、高いパフォーマンスを要求するタスクにおいては、配列やハッシュテーブル（`Data.HashMap`を通じて）がO(1)のアクセス時間でより良いパフォーマンスを提供するかもしれません。

Haskellのエコシステムは、さまざまなニーズに合ったデータ構造を提供しており、`Data.Map`は使いやすさ、柔軟性、パフォーマンスをバランスよく兼ね備えた連想配列に対して優れた汎用的な選択肢です。
