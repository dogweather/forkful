---
date: 2024-01-26 03:47:36.970399-07:00
description: "\u65B9\u6CD5\uFF1A Swift\u3067\u306F\u3001\u6570\u5024\u3092\u4E38\u3081\
  \u308B\u3044\u304F\u3064\u304B\u306E\u65B9\u6CD5\u304C\u63D0\u4F9B\u3055\u308C\u3066\
  \u3044\u307E\u3059\u3002\u3053\u3061\u3089\u304C\u4E00\u4F8B\u3067\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.605551-06:00'
model: gpt-4-0125-preview
summary: "Swift\u3067\u306F\u3001\u6570\u5024\u3092\u4E38\u3081\u308B\u3044\u304F\u3064\
  \u304B\u306E\u65B9\u6CD5\u304C\u63D0\u4F9B\u3055\u308C\u3066\u3044\u307E\u3059\u3002\
  \u3053\u3061\u3089\u304C\u4E00\u4F8B\u3067\u3059\uFF1A."
title: "\u6570\u5024\u306E\u4E38\u3081\u51E6\u7406"
weight: 13
---

## 方法：
Swiftでは、数値を丸めるいくつかの方法が提供されています。こちらが一例です：

```Swift
let original = 3.14159

// 標準の丸め
let standardRounded = round(original) // 3.0

// 特定の小数点以下での丸め
let decimalRounded = Double(round(original * 1000) / 1000) // 3.142

// 切り捨て
let roundedDown = floor(original) // 3.0

// 切り上げ
let roundedUp = ceil(original) // 4.0

print("Standard: \(standardRounded), Decimal: \(decimalRounded), Down: \(roundedDown), Up: \(roundedUp)")
```

出力：`Standard: 3.0, Decimal: 3.142, Down: 3.0, Up: 4.0`

## 詳細解説
歴史的には、丸めはコンピュータより前からある数学の概念で、商業や科学に欠かせないものです。Swiftの`Foundation`フレームワークは、包括的な丸め機能を提供します：

- `round(_: )`は良く知られた半分上げの丸めです。
- `floor(_: )`と`ceil(_: )`は方向性のある丸めを扱います。
- `rounded(.up/.down/.toNearestOrAwayFromZero)`は丸めルールの列挙型を用いて、より細かい制御を可能にします。

精密な財務計算のための`Decimal`型に注意し、浮動小数点のエラーを避けましょう。また、Objective-Cとの互換性のために`NSDecimalNumber`も探求してください。

## 参照
- 浮動小数点算術のためのIEEE標準 (IEEE 754): [IEEE 754](https://ieeexplore.ieee.org/document/4610935)
