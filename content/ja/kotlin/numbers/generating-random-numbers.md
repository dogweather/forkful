---
date: 2024-01-27 20:34:38.416294-07:00
description: "\u65B9\u6CD5: Kotlin\u306F\u3001\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\
  \u30EA\u3092\u901A\u3058\u3066\u4E71\u6570\u3092\u751F\u6210\u3059\u308B\u30B7\u30F3\
  \u30D7\u30EB\u306A\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\u3002\
  \u3053\u3053\u3067\u306F\u3001\u3055\u307E\u3056\u307E\u306A\u30BF\u30A4\u30D7\u306E\
  \u4E71\u6570\u3092\u751F\u6210\u3059\u308B\u65B9\u6CD5\u3092\u8AAC\u660E\u3057\u307E\
  \u3059\uFF1A \u7279\u5B9A\u306E\u7BC4\u56F2\u5185\u3067\u30E9\u30F3\u30C0\u30E0\u306A\
  \u6574\u6570\u3092\u751F\u6210\u3059\u308B\u306B\u306F\uFF1A."
lastmod: '2024-03-13T22:44:42.059083-06:00'
model: gpt-4-0125-preview
summary: "Kotlin\u306F\u3001\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u3092\u901A\
  \u3058\u3066\u4E71\u6570\u3092\u751F\u6210\u3059\u308B\u30B7\u30F3\u30D7\u30EB\u306A\
  \u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\u3002\u3053\u3053\u3067\
  \u306F\u3001\u3055\u307E\u3056\u307E\u306A\u30BF\u30A4\u30D7\u306E\u4E71\u6570\u3092\
  \u751F\u6210\u3059\u308B\u65B9\u6CD5\u3092\u8AAC\u660E\u3057\u307E\u3059\uFF1A\n\
  \n\u7279\u5B9A\u306E\u7BC4\u56F2\u5185\u3067\u30E9\u30F3\u30C0\u30E0\u306A\u6574\
  \u6570\u3092\u751F\u6210\u3059\u308B\u306B\u306F\uFF1A."
title: "\u4E71\u6570\u306E\u751F\u6210"
weight: 12
---

## 方法:
Kotlinは、標準ライブラリを通じて乱数を生成するシンプルな方法を提供しています。ここでは、さまざまなタイプの乱数を生成する方法を説明します：

### ランダムな整数を生成
特定の範囲内でランダムな整数を生成するには：

```kotlin
import kotlin.random.Random

fun main() {
    val randomNumber = Random.nextInt(1, 100) // 1から99の間のランダムな数を生成
    println(randomNumber)
}
```

### ランダムなDoubleを生成
同様にして、ランダムなDoubleを生成します：

```kotlin
import kotlin.random.Random

fun main() {
    val randomDouble = Random.nextDouble(1.0, 10.0) // 1.0から10.0の間のランダムなDoubleを生成
    println(randomDouble)
}
```

### ランダムなBooleanを生成
ランダムなBoolean値を生成するには：

```kotlin
import kotlin.random.Random

fun main() {
    val randomBoolean = Random.nextBoolean() // trueかfalseをランダムに生成
    println(randomBoolean)
}
```

### 再現可能な結果のためのシード設定
テストなど、乱数のシーケンスを再現可能にする必要がある場合は、乱数生成器をシードで設定できます：

```kotlin
import kotlin.random.Random

fun main() {
    val seed = 12345L
    val random = Random(seed)
    val randomNumber = random.nextInt(1, 100)
    println(randomNumber)
}
```

## 詳細解説
Kotlin標準ライブラリによる乱数生成のアプローチは、内部的にJavaの`java.util.Random`を利用しており、使いやすさとパフォーマンスのバランスを実現しています。しかし、これらのメソッドが生成する乱数は疑似乱数であることに注意が必要です。これは、数がランダムに見えるものの、決定論的なプロセスを使用して生成されることを意味します。

ほとんどのアプリケーションでは、Kotlinの`Random`クラスによって提供されるランダム性は十分です。しかし、ランダム性の品質が極めて重要な、例えば暗号学などのセキュリティに敏感なアプリケーションでは、代わりに`java.security.SecureRandom`の使用を検討すべきです。SecureRandomは、より高品質のランダム性を提供するように特別に設計されており、暗号化操作に適していますが、パフォーマンスのトレードオフがある場合があります。

Kotlinは車輪の再発明を行わず、Javaの乱数生成メカニズムにKotlinフレンドリーなAPIを提供し、Kotlinプロジェクト内での使用をより慣用的で簡潔にします。いつものように、ランダム性を扱う際には、プログラマーは用途を慎重に考慮して、ジョブに最も適したツールを選択するべきです。
