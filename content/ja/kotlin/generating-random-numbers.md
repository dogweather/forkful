---
title:                "Kotlin: ランダムな数値を生成する"
simple_title:         "ランダムな数値を生成する"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

乱数を生成する際に人々が参加する理由は、プログラムでの決定要因やランダムな要素を必要とするゲームやシミュレーションを作成するためです。

## 方法

```
Kotlin fun generateRandomNumber() {
    val randomNumber = (0..100).random()
    println("ランダムな数字は " + randomNumber + "です！")
}

```
出力: ランダムな数字は 47 です！

## 詳細を深堀り

乱数を生成するには、基本的にはMath.randomからJavaのRandomクラスを使う方法と、Kotlinの標準ライブラリであるRandomクラスを使う方法があります。また、範囲を指定することで特定の範囲内の乱数を生成することもできます。ただし、プログラム実行中に乱数を生成することは不可能であり、あらかじめ設定したシード値に基づいて計算された固定の乱数列を生成することになります。

## See Also

- [JavaのRandomクラスを使用したランダムな数字の生成方法](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [KotlinのRandomクラスを使用したランダムな数字の生成方法](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- [乱数を生成する際の注意点](https://dzone.com/articles/random-skipping)