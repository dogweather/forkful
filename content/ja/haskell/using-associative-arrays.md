---
title:                "連想配列の使用"
date:                  2024-01-30T19:11:56.507169-07:00
model:                 gpt-4-0125-preview
simple_title:         "連想配列の使用"

category:             "Haskell"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/using-associative-arrays.md"
changelog:
  - 2024-01-30, dogweather, reviewed
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
