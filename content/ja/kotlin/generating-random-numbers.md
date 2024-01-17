---
title:                "ランダムな数値の生成"
html_title:           "Kotlin: ランダムな数値の生成"
simple_title:         "ランダムな数値の生成"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

# ジェネレートするのは何？
ジェネレートとは、ランダムに数値を生成することです。プログラマーがこの機能を利用する理由には、データをランダムに処理したい場合や、ランダムなアイテムを生成したい場合などがあります。

## やり方：
```Kotlin
// ランダムな数値を生成する
val random = Random.nextInt()
// 指定した範囲内のランダムな数値を生成する
val randomInRange = Random.nextInt(0, 100)

println(random) // 出力：-1357832954
println(randomInRange) // 出力：35
```

## 深層:
ジェネレートの歴史的な背景には、様々なアルゴリズムがあります。Kotlinでは、```Random```クラスを使用してランダムな数値を生成することができますが、他の言語では異なる方法で処理される場合もあります。また、ランダムな数値を生成する代替手段として、ハードウェアによるランダム性を利用する方法もあります。

## 関連リンク:
- [KotlinのRandomクラスのドキュメント](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- [ランダム性を利用する方法(英語記事)](https://www.geeksforgeeks.org/generating-random-numbers-in-java/)