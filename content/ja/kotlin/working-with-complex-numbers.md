---
title:                "複素数の扱い方"
date:                  2024-01-26T04:43:12.109369-07:00
model:                 gpt-4-0125-preview
simple_title:         "複素数の扱い方"

category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/working-with-complex-numbers.md"
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
