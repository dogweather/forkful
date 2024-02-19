---
aliases:
- /ja/kotlin/generating-random-numbers/
date: 2024-01-27 20:34:38.416294-07:00
description: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3044\u3066\u4E71\
  \u6570\u306E\u751F\u6210\u306F\u3001\u4E88\u6E2C\u53EF\u80FD\u306A\u30D1\u30BF\u30FC\
  \u30F3\u304C\u306A\u3044\u6570\u3092\u4F5C\u308A\u51FA\u3059\u3053\u3068\u306B\u3064\
  \u3044\u3066\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B7\
  \u30DF\u30E5\u30EC\u30FC\u30B7\u30E7\u30F3\u3001\u30A2\u30EB\u30B4\u30EA\u30BA\u30E0\
  \u306E\u30C6\u30B9\u30C8\u3001\u30B2\u30FC\u30E0\u3001\u305D\u3057\u3066\u30BB\u30AD\
  \u30E5\u30EA\u30C6\u30A3\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306A\u3069\
  \u3001\u3055\u307E\u3056\u307E\u306A\u7406\u7531\u3067\u3053\u308C\u3092\u884C\u3044\
  \u307E\u3059\u3002\u3053\u3053\u3067\u306F\u3001\u73FE\u5B9F\u7684\u307E\u305F\u306F\
  \u5B89\u5168\u306A\u7D50\u679C\u3092\u9054\u6210\u3059\u308B\u305F\u3081\u306B\u4E88\
  \u6E2C\u4E0D\u53EF\u80FD\u6027\u304C\u9375\u3068\u306A\u308A\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:54.875492
model: gpt-4-0125-preview
summary: "\u30D7\u30ED\u30B0\u30E9\u30DF\u30F3\u30B0\u306B\u304A\u3044\u3066\u4E71\
  \u6570\u306E\u751F\u6210\u306F\u3001\u4E88\u6E2C\u53EF\u80FD\u306A\u30D1\u30BF\u30FC\
  \u30F3\u304C\u306A\u3044\u6570\u3092\u4F5C\u308A\u51FA\u3059\u3053\u3068\u306B\u3064\
  \u3044\u3066\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B7\
  \u30DF\u30E5\u30EC\u30FC\u30B7\u30E7\u30F3\u3001\u30A2\u30EB\u30B4\u30EA\u30BA\u30E0\
  \u306E\u30C6\u30B9\u30C8\u3001\u30B2\u30FC\u30E0\u3001\u305D\u3057\u3066\u30BB\u30AD\
  \u30E5\u30EA\u30C6\u30A3\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306A\u3069\
  \u3001\u3055\u307E\u3056\u307E\u306A\u7406\u7531\u3067\u3053\u308C\u3092\u884C\u3044\
  \u307E\u3059\u3002\u3053\u3053\u3067\u306F\u3001\u73FE\u5B9F\u7684\u307E\u305F\u306F\
  \u5B89\u5168\u306A\u7D50\u679C\u3092\u9054\u6210\u3059\u308B\u305F\u3081\u306B\u4E88\
  \u6E2C\u4E0D\u53EF\u80FD\u6027\u304C\u9375\u3068\u306A\u308A\u307E\u3059\u3002"
title: "\u4E71\u6570\u306E\u751F\u6210"
---

{{< edit_this_page >}}

## 何となぜ？

プログラミングにおいて乱数の生成は、予測可能なパターンがない数を作り出すことについてです。プログラマーは、シミュレーション、アルゴリズムのテスト、ゲーム、そしてセキュリティアプリケーションなど、さまざまな理由でこれを行います。ここでは、現実的または安全な結果を達成するために予測不可能性が鍵となります。

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
