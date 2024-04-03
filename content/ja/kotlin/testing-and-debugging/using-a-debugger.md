---
date: 2024-01-26 03:50:47.216912-07:00
description: "\u65B9\u6CD5\uFF1A \u4EE5\u4E0B\u306FIntelliJ IDEA\u3092\u4F7F\u3063\
  \u305FKotlin\u3067\u306E\u30C7\u30D0\u30C3\u30B0\u306E\u4E00\u7AEF\u3067\u3059 -\
  \ IDE\u306E\u30B7\u30E3\u30FC\u30ED\u30C3\u30AF\u30FB\u30DB\u30FC\u30E0\u30BA."
lastmod: '2024-03-13T22:44:42.070678-06:00'
model: gpt-4-0125-preview
summary: "\u4EE5\u4E0B\u306FIntelliJ IDEA\u3092\u4F7F\u3063\u305FKotlin\u3067\u306E\
  \u30C7\u30D0\u30C3\u30B0\u306E\u4E00\u7AEF\u3067\u3059 - IDE\u306E\u30B7\u30E3\u30FC\
  \u30ED\u30C3\u30AF\u30FB\u30DB\u30FC\u30E0\u30BA."
title: "\u30C7\u30D0\u30C3\u30AC\u30FC\u306E\u4F7F\u3044\u65B9"
weight: 35
---

## 方法：
以下はIntelliJ IDEAを使ったKotlinでのデバッグの一端です - IDEのシャーロック・ホームズ:

```kotlin
fun main() {
    val mysteryNumber = 42
    var guess = 0

    while (guess != mysteryNumber) {
        println("数を当ててください: ")
        guess = readLine()?.toIntOrNull() ?: continue // 不正な入力を無視

        // 'guess'の動きを観察するためにここにブレークポイントを設定する
        if (guess < mysteryNumber) {
            println("低すぎます！")
        } else if (guess > mysteryNumber) {
            println("高すぎます！")
        }
    }

    println("当たり！謎の数は$mysteryNumberでした")
}
```

デバッガーの出力:
```
数を当ててください: 
10
低すぎます！
数を当ててください: 
50
高すぎます！
数を当ててください: 
42
当たり！謎の数は42でした
```

## 深堀り
デバッガーは50年代からゲームに参加しています。当時はかなり原始的で、デバッグはソフトウェアよりもハードウェアについてのものであることが多かったです。現在では、IntelliJ IDEAのようなデバッガーにより、ブレークポイントを設定し、コードを一行ずつ進め、変数の状態を自由に調べることができます。

IntelliJのデバッガーはKotlinにとって非常に便利ですが、それだけが選択肢ではありません。Android開発のためのLogcatや、最小限主義者向けのコマンドラインツールであるjdbなど、様々な代替手段があります。ここでの裏側の魔法は主にJVMツールインターフェイス（JVMTI）に関するもので、これによりデバッガーはJava仮想マシンと対話し、Kotlin開発者をループ内に保っています。

## 参照
- IntelliJ IDEAデバッガーのドキュメント: [https://jetbrains.com/idea/](https://www.jetbrains.com/idea/features/debugger.html)
