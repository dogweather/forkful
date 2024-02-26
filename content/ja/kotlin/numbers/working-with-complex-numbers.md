---
date: 2024-01-26 04:43:12.109369-07:00
description: "\u8907\u7D20\u6570\u306F\u3001\u8CA0\u306E\u6570\u306E\u5E73\u65B9\u6839\
  \u3092\u542B\u3080\u3088\u3046\u306B\u6570\u4F53\u7CFB\u3092\u62E1\u5F35\u3057\u307E\
  \u3059\u3002\u3053\u3053\u3067\u3001'\u865A\u6570' \u5358\u4F4D i \u306F -1 \u306E\
  \u5E73\u65B9\u6839\u306B\u7B49\u3057\u3044\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u6CE2\u3001\u632F\u52D5\u3001\u305D\u3057\u3066\u4F55\u304B\
  \u304C\u56DE\u8EE2\u3059\u308B\u3082\u306E\u3092\u30E2\u30C7\u30EA\u30F3\u30B0\u3059\
  \u308B\u306E\u306B\u7D20\u6674\u3089\u3057\u3044\u305F\u3081\u3001\u5DE5\u5B66\u3001\
  \u7269\u7406\u5B66\u3001\u4FE1\u53F7\u51E6\u7406\u306E\u3088\u3046\u306A\u5206\u91CE\
  \u3067\u305D\u308C\u3089\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
lastmod: '2024-02-25T18:49:40.081189-07:00'
model: gpt-4-0125-preview
summary: "\u8907\u7D20\u6570\u306F\u3001\u8CA0\u306E\u6570\u306E\u5E73\u65B9\u6839\
  \u3092\u542B\u3080\u3088\u3046\u306B\u6570\u4F53\u7CFB\u3092\u62E1\u5F35\u3057\u307E\
  \u3059\u3002\u3053\u3053\u3067\u3001'\u865A\u6570' \u5358\u4F4D i \u306F -1 \u306E\
  \u5E73\u65B9\u6839\u306B\u7B49\u3057\u3044\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\
  \u30DE\u30FC\u306F\u3001\u6CE2\u3001\u632F\u52D5\u3001\u305D\u3057\u3066\u4F55\u304B\
  \u304C\u56DE\u8EE2\u3059\u308B\u3082\u306E\u3092\u30E2\u30C7\u30EA\u30F3\u30B0\u3059\
  \u308B\u306E\u306B\u7D20\u6674\u3089\u3057\u3044\u305F\u3081\u3001\u5DE5\u5B66\u3001\
  \u7269\u7406\u5B66\u3001\u4FE1\u53F7\u51E6\u7406\u306E\u3088\u3046\u306A\u5206\u91CE\
  \u3067\u305D\u308C\u3089\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"
title: "\u8907\u7D20\u6570\u306E\u6271\u3044\u65B9"
---

{{< edit_this_page >}}

## 何となぜ？
複素数は、負の数の平方根を含むように数体系を拡張します。ここで、'虚数' 単位 i は -1 の平方根に等しいです。プログラマーは、波、振動、そして何かが回転するものをモデリングするのに素晴らしいため、工学、物理学、信号処理のような分野でそれらを使用します。

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
