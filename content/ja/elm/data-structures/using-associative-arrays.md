---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:57.753136-07:00
description: "\u9023\u60F3\u914D\u5217\u3001\u307E\u305F\u306FElm\u304C\u547C\u3093\
  \u3067\u3044\u308B\u3088\u3046\u306B\u3001\u8F9E\u66F8\u306F\u30AD\u30FC\u3092\u5024\
  \u306B\u30DE\u30C3\u30D4\u30F3\u30B0\u3057\u3001\u5024\u306E\u691C\u7D22\u3001\u633F\
  \u5165\u3001\u524A\u9664\u3092\u975E\u5E38\u306B\u901F\u304F\u3057\u307E\u3059\u3002\
  \u30E6\u30FC\u30B6\u30FC\u306E\u597D\u307F\u3084\u5728\u5EAB\u30EA\u30B9\u30C8\u306E\
  \u3088\u3046\u306B\u3001\u53B3\u5BC6\u306A\u9806\u5E8F\u3092\u5FC5\u8981\u3068\u3057\
  \u306A\u3044\u3082\u306E\u3092\u8FFD\u8DE1\u3059\u308B\u5FC5\u8981\u304C\u3042\u308B\
  \u3068\u304D\u306B\u4FBF\u5229\u3067\u3059\u3002"
lastmod: '2024-03-13T22:44:41.998568-06:00'
model: gpt-4-0125-preview
summary: "\u9023\u60F3\u914D\u5217\u3001\u307E\u305F\u306FElm\u304C\u547C\u3093\u3067\
  \u3044\u308B\u3088\u3046\u306B\u3001\u8F9E\u66F8\u306F\u30AD\u30FC\u3092\u5024\u306B\
  \u30DE\u30C3\u30D4\u30F3\u30B0\u3057\u3001\u5024\u306E\u691C\u7D22\u3001\u633F\u5165\
  \u3001\u524A\u9664\u3092\u975E\u5E38\u306B\u901F\u304F\u3057\u307E\u3059\u3002\u30E6\
  \u30FC\u30B6\u30FC\u306E\u597D\u307F\u3084\u5728\u5EAB\u30EA\u30B9\u30C8\u306E\u3088\
  \u3046\u306B\u3001\u53B3\u5BC6\u306A\u9806\u5E8F\u3092\u5FC5\u8981\u3068\u3057\u306A\
  \u3044\u3082\u306E\u3092\u8FFD\u8DE1\u3059\u308B\u5FC5\u8981\u304C\u3042\u308B\u3068\
  \u304D\u306B\u4FBF\u5229\u3067\u3059\u3002."
title: "\u9023\u60F3\u914D\u5217\u306E\u4F7F\u7528"
weight: 15
---

## どのようにして：
Elmでは、`Dict`モジュールで辞書と作業しますので、簡単な例に飛び込んでみましょう：

```Elm
import Dict exposing (Dict)

-- 文字列キーとInt値を持つ辞書の初期化
exampleDict : Dict String Int
exampleDict = Dict.fromList [("apple", 5), ("banana", 2), ("orange", 8)]

-- 値を追加または更新
updatedDict = Dict.insert "grape" 10 exampleDict

-- 値を取得（キーが存在しない可能性があるため、Maybe型に注意）
fruitCount : Maybe Int
fruitCount = Dict.get "apple" updatedDict

-- キー値ペアを削除
finalDict = Dict.remove "banana" updatedDict

-- 辞書をリストに戻す
dictToList = Dict.toList finalDict
```

`dictToList`を表示したときのサンプル出力：

```Elm
[("apple", 5), ("grape", 10), ("orange", 8)]
```

これは、辞書の作成、更新、アクセス、およびイテレートの基本的な操作を示しています。

## 深く掘り下げて
Elmの中での辞書は内部的にAVL木として知られる構造を使用しています - 自己バランスを取るバイナリ検索木の一種です。この選択は、insert、getおよびremoveのような操作が良いパフォーマンス（対数時間複雑度）を保証し、データの処理を簡素化するバランスをとります。

Elmの`Dict`の強みにもかかわらず、すべての状況に適しているわけではありません。順序付けされたコレクションや、連続してイテレートする必要がある場合、リストや配列の方が適しているかもしれません。さらに、既知の固定セットのキーで作業する場合、カスタムタイプ（Elmの列挙のバージョン）を使用することで、より多くの型の安全性とコード内の明確な意図を提供できます。

Elmのエコシステムでは、`Dict`はキーがユニークで順序が重要でないキー値ペアのコレクションを管理する信頼できる方法を提供します。より新しいまたはより洗練された構造が出現するかもしれませんが、`Dict`モジュールはElmプログラマーのツールキットの基本的なツールとして、連想配列を扱う際のシンプルさと効率でその地位を保持します。
