---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:26.553571-07:00
description: "\u4F7F\u3044\u65B9: Swift\u3067\u306F\u3001\u9023\u60F3\u914D\u5217\u3092\
  \u4F7F\u3046\u306E\u304C\u76F4\u611F\u7684\u3067\u3059\u3002\u4EE5\u4E0B\u306F\u3001\
  Swift\u8F9E\u66F8\u3067\u30A2\u30A4\u30C6\u30E0\u3092\u5BA3\u8A00\u3057\u3001\u8FFD\
  \u52A0\u3057\u3001\u524A\u9664\u3057\u3001\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u65B9\
  \u6CD5\u3067\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.602547-06:00'
model: gpt-4-0125-preview
summary: "Swift\u3067\u306F\u3001\u9023\u60F3\u914D\u5217\u3092\u4F7F\u3046\u306E\u304C\
  \u76F4\u611F\u7684\u3067\u3059\u3002\u4EE5\u4E0B\u306F\u3001Swift\u8F9E\u66F8\u3067\
  \u30A2\u30A4\u30C6\u30E0\u3092\u5BA3\u8A00\u3057\u3001\u8FFD\u52A0\u3057\u3001\u524A\
  \u9664\u3057\u3001\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u65B9\u6CD5\u3067\u3059\uFF1A\
  ."
title: "\u9023\u60F3\u914D\u5217\u306E\u4F7F\u7528"
weight: 15
---

## 使い方:
Swiftでは、連想配列を使うのが直感的です。以下は、Swift辞書でアイテムを宣言し、追加し、削除し、アクセスする方法です：

```Swift
// 辞書の宣言
var fruitColors: [String: String] = ["Apple": "Red", "Banana": "Yellow"]

// 新しいアイテムの追加
fruitColors["Grape"] = "Purple"

// キーを使用して値にアクセス
if let appleColor = fruitColors["Apple"] {
    print("Apple is \(appleColor).")  // 出力: Apple is Red.
} else {
    print("色が見つかりません。")
}

// アイテムの削除
fruitColors["Banana"] = nil  // これにより辞書から "Banana" が削除されます

// アイテムの繰り返し処理
for (fruit, color) in fruitColors {
    print("\(fruit) is \(color).")
    // 出力:
    // Apple is Red.
    // Grape is Purple.
}
```

辞書は非常に多様で、動的にデータを操作してアクセスすることができます。データの取得速度に影響を与えないその順序付けされていない性質は、大量のデータセットを扱う場合に重要な利点です。

## 深堀り
連想配列としての辞書のSwiftによる実装は、一意のキーに値をマップする強力な能力から来ています。歴史的に、プログラミング言語は、キーと値の間の「マップ」を作成する機能を示唆する、ハッシュテーブルやマップなど様々な名前でこの概念を実装してきました。

Swiftにおける辞書は、効率的なデータ取得のためにハッシュ化可能なキーを活用してパフォーマンスが最適化されています。これは、`[Key: Value]` 辞書の `Key` 型が `Hashable` プロトコルに準拠している必要があることを意味します。これは、`Int`、`String`、`Double` などのほとんどのSwift標準型で事例です。

考慮すべき一つのことは、辞書はデータのペアを関連付けるのに優れていますが、順序がないということです。要素の順序を維持する必要がある場合は、順序付き要素のシーケンスに対して `Array`や、配列と辞書の両方の特徴を組み合わせたカスタムデータ構造を探求することができます。

また、Swiftは常に進化しており、それに伴って辞書の取り扱いや最適化も進化しています。したがって、辞書を最大限に活用するためには、最新のSwiftドキュメントに常に更新しておくことが重要であり、最も効率的で最新の実践を使用していることを確認します。
