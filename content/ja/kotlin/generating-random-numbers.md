---
title:                "ランダム数の生成"
date:                  2024-01-20T17:49:29.824106-07:00
model:                 gpt-4-1106-preview
simple_title:         "ランダム数の生成"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?（なに？となぜ？）
乱数生成とは、予測不可能な数を生み出すこと。プログラマーはテスト、ゲーム、セキュリティ、シミュレーションのためにこれを使う。

## How to:（方法）
```kotlin
import kotlin.random.Random

fun main() {
    // 範囲内のランダムな整数を得る
    val randomInt = Random.nextInt(0, 100)
    println(randomInt)

    // ランダムなBooleanを得る
    val randomBoolean = Random.nextBoolean()
    println(randomBoolean)

    // ランダムな浮動小数点数を得る
    val randomDouble = Random.nextDouble(1.0, 10.0)
    println(randomDouble)
}

// 出力例:
// 42
// true
// 5.8372016572
```

## Deep Dive（掘り下げ）
乱数はプログラミングの歴史の中で長い道のりを歩んできた。初期には単純なアルゴリズムが使われていたが、今日では暗号学的に安全な乱数生成方法が一般的。Kotlinでは`java.util.Random`クラスから派生した`kotlin.random.Random`を使用。シーディングや分布などの実装詳細はランダム性の質に影響を及ぼす。また、`ThreadLocalRandom`や`SecureRandom`などの代替手段もある。

## See Also（関連リンク）
- Kotlinの公式ドキュメント: [Random](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- `ThreadLocalRandom`の使用方法: [ThreadLocalRandom](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ThreadLocalRandom.html)
- `SecureRandom`とは: [SecureRandom](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)