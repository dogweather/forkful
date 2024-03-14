---
date: 2024-01-26 03:46:01.009833-07:00
description: "\u6570\u5024\u3092\u4E38\u3081\u308B\u3068\u3044\u3046\u306E\u306F\u3001\
  \u305D\u308C\u3089\u3092\u6700\u3082\u8FD1\u3044\u6574\u6570\u304B\u3001\u6307\u5B9A\
  \u3055\u308C\u305F\u7CBE\u5EA6\u306E\u5EA6\u5408\u3044\u306B\u8ABF\u6574\u3059\u308B\
  \u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u304C\u6570\u5024\u3092\u4E38\u3081\u308B\u306E\u306F\u3001\u53EF\u8AAD\u6027\
  \u3092\u5411\u4E0A\u3055\u305B\u305F\u308A\u3001\u30B9\u30C8\u30EC\u30FC\u30B8\u8981\
  \u4EF6\u3092\u524A\u6E1B\u3057\u305F\u308A\u3001\u6B63\u78BA\u306A\u5024\u304C\u5F8C\
  \u7D9A\u306E\u8A08\u7B97\u3067\u91CD\u8981\u3067\u306F\u306A\u3044\u5834\u5408\u304C\
  \u3042\u308A\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.057460-06:00'
model: gpt-4-0125-preview
summary: "\u6570\u5024\u3092\u4E38\u3081\u308B\u3068\u3044\u3046\u306E\u306F\u3001\
  \u305D\u308C\u3089\u3092\u6700\u3082\u8FD1\u3044\u6574\u6570\u304B\u3001\u6307\u5B9A\
  \u3055\u308C\u305F\u7CBE\u5EA6\u306E\u5EA6\u5408\u3044\u306B\u8ABF\u6574\u3059\u308B\
  \u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u304C\u6570\u5024\u3092\u4E38\u3081\u308B\u306E\u306F\u3001\u53EF\u8AAD\u6027\
  \u3092\u5411\u4E0A\u3055\u305B\u305F\u308A\u3001\u30B9\u30C8\u30EC\u30FC\u30B8\u8981\
  \u4EF6\u3092\u524A\u6E1B\u3057\u305F\u308A\u3001\u6B63\u78BA\u306A\u5024\u304C\u5F8C\
  \u7D9A\u306E\u8A08\u7B97\u3067\u91CD\u8981\u3067\u306F\u306A\u3044\u5834\u5408\u304C\
  \u3042\u308A\u307E\u3059\u3002"
title: "\u6570\u5024\u306E\u4E38\u3081\u51E6\u7406"
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
