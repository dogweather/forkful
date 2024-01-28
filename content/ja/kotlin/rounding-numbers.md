---
title:                "数値の丸め処理"
date:                  2024-01-26T03:46:01.009833-07:00
model:                 gpt-4-0125-preview
simple_title:         "数値の丸め処理"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/rounding-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？

数値を丸めるというのは、それらを最も近い整数か、指定された精度の度合いに調整することを意味します。プログラマーが数値を丸めるのは、可読性を向上させたり、ストレージ要件を削減したり、正確な値が後続の計算で重要ではない場合があります。

## 方法:

Kotlinでは、`roundToInt()`, `roundToDouble()`, そしてより制御された`BigDecimal`を使用して丸めることができます:

```kotlin
fun main() {
    val number1 = 3.14159
    println(number1.roundToInt()) // 出力: 3

    val number2 = 3.5
    println(number2.roundToInt()) // 出力: 4

    val number3 = 123.456
    println("%.2f".format(number3)) // 出力: 123.46
    
    val bigDecimal = number3.toBigDecimal().setScale(1, RoundingMode.HALF_EVEN)
    println(bigDecimal) // 出力: 123.5
}
```

## 詳細解説

歴史的に、数値を丸めるという概念は、数値精度の限界を扱うために、数学および計算の両方で基本的な概念でした。初期の計算において、メモリの高価さのため、丸めが重要でした。

Kotlinでの丸めは、標準的なJavaライブラリに基づいています。丸めのオプションには、最も近い整数に丸める`Math.round()`や、スケールと`RoundingMode`を指定できるカスタマイズ可能な丸めのための`BigDecimal`があります。

各`RoundingMode`には、丸めるオプションの真ん中にちょうどある数字（タイ）を処理するための異なる方針があります。たとえば、`RoundingMode.HALF_UP`は最も近い隣の数字に丸めますが、隣が等距離にある場合は上に丸めます。

## 参考

- Kotlinの[`BigDecimal`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/java.math.-big-decimal/index.html)に関するドキュメント
- OracleのJavaドキュメントにおける[`RoundingMode`](https://docs.oracle.com/javase/8/docs/api/java/math/RoundingMode.html)
- 浮動小数点算術のIEEE標準 (IEEE 754) [IEEE標準754](https://ieeexplore.ieee.org/document/4610935)
