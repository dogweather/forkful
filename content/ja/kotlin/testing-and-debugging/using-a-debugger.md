---
date: 2024-01-26 03:50:47.216912-07:00
description: "\u30C7\u30D0\u30C3\u30AC\u30FC\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\
  \u306F\u3001\u30B3\u30FC\u30C9\u3092\u4E00\u6B69\u4E00\u6B69\u9032\u3081\u3001\u52D5\
  \u4F5C\u3092\u89B3\u5BDF\u3057\u3001\u5384\u4ECB\u306A\u30D0\u30B0\u3092\u8D64\u88F8\
  \u3005\u306B\u6355\u307E\u3048\u308B\u3053\u3068\u306B\u3064\u3044\u3066\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u30C7\u30D0\u30C3\u30AC\u30FC\u3092\
  \u4F7F\u7528\u3059\u308B\u306E\u306F\u3001\u9AEA\u3092\u629C\u304F\u3053\u3068\u306A\
  \u304F\u4F55\u304C\u9593\u9055\u3063\u3066\u3044\u308B\u306E\u304B\u3092\u89E3\u660E\
  \u3059\u308B\u306E\u306B\u5F79\u7ACB\u3064\u63A2\u5075\u30C4\u30FC\u30EB\u3060\u304B\
  \u3089\u3067\u3059\u3002"
lastmod: '2024-02-25T18:49:40.094864-07:00'
model: gpt-4-0125-preview
summary: "\u30C7\u30D0\u30C3\u30AC\u30FC\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\
  \u306F\u3001\u30B3\u30FC\u30C9\u3092\u4E00\u6B69\u4E00\u6B69\u9032\u3081\u3001\u52D5\
  \u4F5C\u3092\u89B3\u5BDF\u3057\u3001\u5384\u4ECB\u306A\u30D0\u30B0\u3092\u8D64\u88F8\
  \u3005\u306B\u6355\u307E\u3048\u308B\u3053\u3068\u306B\u3064\u3044\u3066\u3067\u3059\
  \u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u30C7\u30D0\u30C3\u30AC\u30FC\u3092\
  \u4F7F\u7528\u3059\u308B\u306E\u306F\u3001\u9AEA\u3092\u629C\u304F\u3053\u3068\u306A\
  \u304F\u4F55\u304C\u9593\u9055\u3063\u3066\u3044\u308B\u306E\u304B\u3092\u89E3\u660E\
  \u3059\u308B\u306E\u306B\u5F79\u7ACB\u3064\u63A2\u5075\u30C4\u30FC\u30EB\u3060\u304B\
  \u3089\u3067\u3059\u3002"
title: "\u30C7\u30D0\u30C3\u30AC\u30FC\u306E\u4F7F\u3044\u65B9"
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
