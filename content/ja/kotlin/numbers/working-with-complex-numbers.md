---
date: 2024-01-26 04:43:12.109369-07:00
description: "\u65B9\u6CD5\uFF1A \u8907\u7D20\u6570\u306F16\u4E16\u7D00\u306B\u521D\
  \u3081\u3066\u8A00\u53CA\u3055\u308C\u3001\u5B9F\u8CEA\u7684\u306A\u89E3\u3092\u6B20\
  \u3044\u3066\u3044\u305F\u7ACB\u65B9\u65B9\u7A0B\u5F0F\u3092\u89E3\u304F\u305F\u3081\
  \u306B\u4F7F\u7528\u3055\u308C\u307E\u3057\u305F\u3002\u5DE5\u5B66\u3068\u7269\u7406\
  \u5B66\u306F\u3001AC\u56DE\u8DEF\u3084\u6CE2\u5F62\u3092\u5206\u6790\u3059\u308B\
  \u305F\u3081\u306B\u8907\u7D20\u6570\u3092\u5E83\u7BC4\u56F2\u306B\u5229\u7528\u3057\
  \u3066\u3044\u307E\u3059\u3002\u30D8\u30D3\u30FC\u30C7\u30E5\u30FC\u30C6\u30A3\u306A\
  \u4F5C\u696D\u7528\u306B\u3001Kotlin\u306E `koma` \u3084 `ejml` \u306E\u3088\u3046\
  \u306A\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4EE3\u308F\u308A\u306B\u4F7F\u7528\u3059\
  \u308B\u3053\u3068\u3082\u3067\u304D\u307E\u3059\u3002\u2026"
lastmod: '2024-04-05T22:50:55.995316-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A \u8907\u7D20\u6570\u306F16\u4E16\u7D00\u306B\u521D\u3081\
  \u3066\u8A00\u53CA\u3055\u308C\u3001\u5B9F\u8CEA\u7684\u306A\u89E3\u3092\u6B20\u3044\
  \u3066\u3044\u305F\u7ACB\u65B9\u65B9\u7A0B\u5F0F\u3092\u89E3\u304F\u305F\u3081\u306B\
  \u4F7F\u7528\u3055\u308C\u307E\u3057\u305F\u3002\u5DE5\u5B66\u3068\u7269\u7406\u5B66\
  \u306F\u3001AC\u56DE\u8DEF\u3084\u6CE2\u5F62\u3092\u5206\u6790\u3059\u308B\u305F\
  \u3081\u306B\u8907\u7D20\u6570\u3092\u5E83\u7BC4\u56F2\u306B\u5229\u7528\u3057\u3066\
  \u3044\u307E\u3059\u3002\u30D8\u30D3\u30FC\u30C7\u30E5\u30FC\u30C6\u30A3\u306A\u4F5C\
  \u696D\u7528\u306B\u3001Kotlin\u306E `koma` \u3084 `ejml` \u306E\u3088\u3046\u306A\
  \u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u4EE3\u308F\u308A\u306B\u4F7F\u7528\u3059\u308B\
  \u3053\u3068\u3082\u3067\u304D\u307E\u3059\u3002 \u8907\u7D20\u6570\u306B\u5BFE\u3059\
  \u308B\u64CD\u4F5C\u306F\u5B9F\u6570\u306B\u5BFE\u3059\u308B\u64CD\u4F5C\u3092\u53CD\
  \u6620\u3057\u3066\u3044\u307E\u3059\u304C\u3001\u865A\u6570\u5358\u4F4D\u306B\u6CE8\
  \u610F\u3092\u6255\u3044\u307E\u3059\u3002\u4F8B\u3048\u3070\u3001\u639B\u3051\u7B97\
  \u306F\u5206\u914D\u6CD5\u5247\u306B\u5F93\u3044\u307E\u3059\u304C\u3001`i^2 = -1`\
  \ \u3067\u3042\u308B\u3053\u3068\u3092\u899A\u3048\u3066\u304A\u304F\u5FC5\u8981\
  \u304C\u3042\u308A\u307E\u3059\u3002\u3053\u306E\u865A\u6570\u5358\u4F4D\u306B\u3088\
  \u308A\u3001\u69D8\u3005\u306A\u79D1\u5B66\u7684\u8A08\u7B97\u306B\u4E0D\u53EF\u6B20\
  \u306A\u3001\u591A\u6B21\u5143\u306E\u6570\u3092\u8868\u73FE\u3059\u308B\u3053\u3068\
  \u304C\u3067\u304D\u307E\u3059\u3002"
title: "\u8907\u7D20\u6570\u306E\u6271\u3044\u65B9"
weight: 14
---

## 方法：
Kotlinで基本的な複素数クラスを定義しましょう：

```kotlin
data class Complex(val real: Double, val imaginary: Double) {
    operator fun plus(other: Complex) = Complex(real + other.real, imaginary + other.imaginary)
    operator fun minus(other: Complex) = Complex(real - other.real, imaginary - other.imaginary)
    operator fun times(other: Complex) = Complex(
        real * other.real - imaginary * other.imaginary,
        real * other.imaginary + imaginary * other.real
    )
    
    override fun toString(): String = "($real + ${imaginary}i)"
}

fun main() {
    val a = Complex(1.0, 2.0)
    val b = Complex(3.0, 4.0)
    
    println("a + b = ${a + b}")  // 出力： a + b = (4.0 + 6.0i)
    println("a - b = ${a - b}")  // 出力： a - b = (-2.0 - 2.0i)
    println("a * b = ${a * b}")  // 出力： a * b = (-5.0 + 10.0i)
}
```

## ディープダイブ
複素数は16世紀に初めて言及され、実質的な解を欠いていた立方方程式を解くために使用されました。工学と物理学は、AC回路や波形を分析するために複素数を広範囲に利用しています。ヘビーデューティな作業用に、Kotlinの `koma` や `ejml` のようなライブラリを代わりに使用することもできます。

複素数に対する操作は実数に対する操作を反映していますが、虚数単位に注意を払います。例えば、掛け算は分配法則に従いますが、`i^2 = -1` であることを覚えておく必要があります。この虚数単位により、様々な科学的計算に不可欠な、多次元の数を表現することができます。

## 関連情報
Kotlin 数学ライブラリ:

- [koma](https://koma.kyonifer.com/): Kotlin のための科学計算ライブラリ。

複素数に関するさらなる読み物:

- [ウィキペディア: 複素数](https://en.wikipedia.org/wiki/Complex_number)
