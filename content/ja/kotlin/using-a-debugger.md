---
title:                "デバッガーの使い方"
date:                  2024-01-26T03:50:47.216912-07:00
model:                 gpt-4-0125-preview
simple_title:         "デバッガーの使い方"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/using-a-debugger.md"
---

{{< edit_this_page >}}

## 何となぜ？
デバッガーを使用することは、コードを一歩一歩進め、動作を観察し、厄介なバグを赤裸々に捕まえることについてです。プログラマーがデバッガーを使用するのは、髪を抜くことなく何が間違っているのかを解明するのに役立つ探偵ツールだからです。

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
