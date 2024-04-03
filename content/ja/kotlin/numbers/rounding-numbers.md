---
date: 2024-01-26 03:46:01.009833-07:00
description: "\u65B9\u6CD5: Kotlin\u3067\u306F\u3001`roundToInt()`, `roundToDouble()`,\
  \ \u305D\u3057\u3066\u3088\u308A\u5236\u5FA1\u3055\u308C\u305F`BigDecimal`\u3092\
  \u4F7F\u7528\u3057\u3066\u4E38\u3081\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\
  ."
lastmod: '2024-03-13T22:44:42.057460-06:00'
model: gpt-4-0125-preview
summary: "Kotlin\u3067\u306F\u3001`roundToInt()`, `roundToDouble()`, \u305D\u3057\u3066\
  \u3088\u308A\u5236\u5FA1\u3055\u308C\u305F`BigDecimal`\u3092\u4F7F\u7528\u3057\u3066\
  \u4E38\u3081\u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059."
title: "\u6570\u5024\u306E\u4E38\u3081\u51E6\u7406"
weight: 13
---

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
