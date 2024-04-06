---
changelog:
- 2024-01-30, dogweather, reviewed
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:11:56.507169-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Haskell\u306B\u306F\
  \u3001\u4ED6\u306E\u8A00\u8A9E\u306E\u3088\u3046\u306B\u305D\u306E\u307E\u307E\u9023\
  \u60F3\u914D\u5217\u304C\u7D44\u307F\u8FBC\u307E\u308C\u3066\u3044\u308B\u308F\u3051\
  \u3067\u306F\u3042\u308A\u307E\u305B\u3093\u304C\u3001\u30AD\u30FC\u3068\u5024\u306E\
  \u30DA\u30A2\u3092\u6271\u3046\u305F\u3081\u306E\u5F37\u529B\u306A\u6A19\u6E96\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u3067\u3042\u308B`Data.Map`\u3092\u63D0\u4F9B\u3057\u3066\
  \u3044\u307E\u3059\u3002\u3055\u3042\u3001\u8896\u3092\u307E\u304F\u3063\u3066\u4F7F\
  \u3044\u65B9\u3092\u898B\u3066\u307F\u307E\u3057\u3087\u3046! \u307E\u305A\u3001\
  \u30A4\u30F3\u30DD\u30FC\u30C8\u3059\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3057\u3066\
  \u304F\u3060\u3055\u3044\uFF1A."
lastmod: '2024-04-05T22:38:41.713531-06:00'
model: gpt-4-0125-preview
summary: "\u3069\u306E\u3088\u3046\u306B\u3057\u3066\uFF1A Haskell\u306B\u306F\u3001\
  \u4ED6\u306E\u8A00\u8A9E\u306E\u3088\u3046\u306B\u305D\u306E\u307E\u307E\u9023\u60F3\
  \u914D\u5217\u304C\u7D44\u307F\u8FBC\u307E\u308C\u3066\u3044\u308B\u308F\u3051\u3067\
  \u306F\u3042\u308A\u307E\u305B\u3093\u304C\u3001\u30AD\u30FC\u3068\u5024\u306E\u30DA\
  \u30A2\u3092\u6271\u3046\u305F\u3081\u306E\u5F37\u529B\u306A\u6A19\u6E96\u30E9\u30A4\
  \u30D6\u30E9\u30EA\u3067\u3042\u308B`Data.Map`\u3092\u63D0\u4F9B\u3057\u3066\u3044\
  \u307E\u3059\u3002\u3055\u3042\u3001\u8896\u3092\u307E\u304F\u3063\u3066\u4F7F\u3044\
  \u65B9\u3092\u898B\u3066\u307F\u307E\u3057\u3087\u3046! \u307E\u305A\u3001\u30A4\
  \u30F3\u30DD\u30FC\u30C8\u3059\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3057\u3066\u304F\
  \u3060\u3055\u3044\uFF1A."
title: "\u9023\u60F3\u914D\u5217\u306E\u4F7F\u7528"
weight: 15
---

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
