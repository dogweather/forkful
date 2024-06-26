---
date: 2024-01-27 20:35:55.805207-07:00
description: "\u65B9\u6CD5\uFF1A Swift\u306F\u3001\u305D\u306E\u6A19\u6E96\u30E9\u30A4\
  \u30D6\u30E9\u30EA\u3092\u901A\u3058\u3066\u4E71\u6570\u3092\u751F\u6210\u3059\u308B\
  \u305F\u3081\u306E\u76F4\u63A5\u7684\u306A\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u3066\
  \u3044\u307E\u3059\u3002\u3053\u3053\u3067\u306F\u3001\u3055\u307E\u3056\u307E\u306A\
  \u6570\u5024\u578B\u306B\u5BFE\u3057\u3066\u305D\u308C\u3092\u3069\u306E\u3088\u3046\
  \u306B\u884C\u3046\u304B\u3092\u8AAC\u660E\u3057\u307E\u3059\uFF1A."
lastmod: '2024-04-05T22:38:42.107786-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Swift\u306F\u3001\u305D\u306E\u6A19\u6E96\u30E9\u30A4\
  \u30D6\u30E9\u30EA\u3092\u901A\u3058\u3066\u4E71\u6570\u3092\u751F\u6210\u3059\u308B\
  \u305F\u3081\u306E\u76F4\u63A5\u7684\u306A\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u3066\
  \u3044\u307E\u3059\u3002\u3053\u3053\u3067\u306F\u3001\u3055\u307E\u3056\u307E\u306A\
  \u6570\u5024\u578B\u306B\u5BFE\u3057\u3066\u305D\u308C\u3092\u3069\u306E\u3088\u3046\
  \u306B\u884C\u3046\u304B\u3092\u8AAC\u660E\u3057\u307E\u3059\uFF1A."
title: "\u4E71\u6570\u306E\u751F\u6210"
weight: 12
---

## 方法：
Swiftは、その標準ライブラリを通じて乱数を生成するための直接的な方法を提供しています。ここでは、さまざまな数値型に対してそれをどのように行うかを説明します：

```Swift
// 0からInt.maxまでのランダムな整数を生成
let randomInt = Int.random(in: 0...Int.max)
print(randomInt)

// 0.0から1.0までのランダムな浮動小数点数を生成
let randomDouble = Double.random(in: 0.0...1.0)
print(randomDouble)

// ランダムなBool値を生成
let randomBool = Bool.random()
print(randomBool)
```

サンプルの出力は変動する可能性があります。なぜなら、結局のところ私たちは乱数を扱っているからです。コードを複数回実行すると、異なる数値やブール値が得られます。

## 詳細分析
Swiftにおける乱数生成のアプローチは、堅牢で効率的な擬似乱数生成器（PRNG）の上に構築されています。Swift 4.2以前は、開発者は外部ライブラリや基盤となるプラットフォームの機能に頼っていましたが、これは異なるプラットフォームや環境間での一貫性のなさにつながることがありました。Swift 4.2でネイティブAPIが導入されて以来、乱数を生成することは、基盤となるプラットフォームに関わらず、よりシンプルで一貫性のあるものとなりました。

しかし、重要なのは、Swiftの標準乱数生成器は暗号化目的には適していないということを理解することです。暗号化のためには、Appleプラットフォームの`Security`フレームワークを使用する必要があり、これは暗号学的に安全なランダムバイトにアクセスする方法を提供します。私の最後の更新時点で、Swiftの標準ライブラリには、クロスプラットフォームの暗号学的乱数生成器は含まれておらず、非Appleプラットフォームでのそのようなニーズに対しては、開発者は第三者のライブラリを求めることになります。

科学計算の領域や、擬似乱数の決定的なシーケンス（そのシーケンスを正確に再現できる場合）が必要な状況では、Swiftの乱数生成は、ジェネレーターのシード設定の機能なしには最適なものではないかもしれません。そのような場合、特別なライブラリやアルゴリズムが、これらの正確な要件を満たすためにしばしば使用されます。
