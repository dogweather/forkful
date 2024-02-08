---
title:                "数値の丸め処理"
aliases:
- ja/swift/rounding-numbers.md
date:                  2024-01-26T03:47:36.970399-07:00
model:                 gpt-4-0125-preview
simple_title:         "数値の丸め処理"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/rounding-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？

数値を丸めるとは、特定の精度に数値を近似することを指し、通常は望ましくない小数点以下を削除するために行います。プログラマーは、メモリ管理、可読性の向上、および通貨制約のようなドメイン固有の要件を満たすために丸めを行います。

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
