---
date: 2024-01-26 03:47:36.970399-07:00
description: "\u6570\u5024\u3092\u4E38\u3081\u308B\u3068\u306F\u3001\u7279\u5B9A\u306E\
  \u7CBE\u5EA6\u306B\u6570\u5024\u3092\u8FD1\u4F3C\u3059\u308B\u3053\u3068\u3092\u6307\
  \u3057\u3001\u901A\u5E38\u306F\u671B\u307E\u3057\u304F\u306A\u3044\u5C0F\u6570\u70B9\
  \u4EE5\u4E0B\u3092\u524A\u9664\u3059\u308B\u305F\u3081\u306B\u884C\u3044\u307E\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30E1\u30E2\u30EA\u7BA1\u7406\
  \u3001\u53EF\u8AAD\u6027\u306E\u5411\u4E0A\u3001\u304A\u3088\u3073\u901A\u8CA8\u5236\
  \u7D04\u306E\u3088\u3046\u306A\u30C9\u30E1\u30A4\u30F3\u56FA\u6709\u306E\u8981\u4EF6\
  \u3092\u6E80\u305F\u3059\u305F\u3081\u306B\u4E38\u3081\u3092\u884C\u3044\u307E\u3059\
  \u3002"
lastmod: 2024-02-19 22:05:01.718136
model: gpt-4-0125-preview
summary: "\u6570\u5024\u3092\u4E38\u3081\u308B\u3068\u306F\u3001\u7279\u5B9A\u306E\
  \u7CBE\u5EA6\u306B\u6570\u5024\u3092\u8FD1\u4F3C\u3059\u308B\u3053\u3068\u3092\u6307\
  \u3057\u3001\u901A\u5E38\u306F\u671B\u307E\u3057\u304F\u306A\u3044\u5C0F\u6570\u70B9\
  \u4EE5\u4E0B\u3092\u524A\u9664\u3059\u308B\u305F\u3081\u306B\u884C\u3044\u307E\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30E1\u30E2\u30EA\u7BA1\u7406\
  \u3001\u53EF\u8AAD\u6027\u306E\u5411\u4E0A\u3001\u304A\u3088\u3073\u901A\u8CA8\u5236\
  \u7D04\u306E\u3088\u3046\u306A\u30C9\u30E1\u30A4\u30F3\u56FA\u6709\u306E\u8981\u4EF6\
  \u3092\u6E80\u305F\u3059\u305F\u3081\u306B\u4E38\u3081\u3092\u884C\u3044\u307E\u3059\
  \u3002"
title: "\u6570\u5024\u306E\u4E38\u3081\u51E6\u7406"
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
