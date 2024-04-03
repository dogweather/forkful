---
date: 2024-01-20 17:35:20.131561-07:00
description: "How to: (\u65B9\u6CD5) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.052785-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
weight: 3
---

## How to: (方法)
```kotlin
fun main() {
    val greeting = "こんにちは、"
    val subject = "世界！"
    val message = greeting + subject  // 文字列をプラス記号で結合
    println(message)  // "こんにちは、世界！" を出力
}
```

```kotlin
fun main() {
    val name = "太郎"
    val age = 25
    val introduction = "$name さん、年齢は $age 歳です。"  // 文字列テンプレートで結合
    println(introduction)  // "太郎 さん、年齢は 25 歳です。" を出力
}
```

## Deep Dive (掘り下げ)
文字列の結合には歴史があります。初期のプログラミング言語では操作がより手間でしたが、現代の言語は簡単に結合を可能にしています。Kotlinではプラス演算子(`+`)や文字列テンプレートを使えるため直感的です。実装の面では、短い文字列の結合は効率が良いですが、多くの文字列や大きなデータを扱う場合は`StringBuilder`を使ったほうが性能が良くなります。Javaの`String`クラスは不変なので、Kotlinも同じバックエンドを利用するため、新しい文字列オブジェクトが結合のたびに生成されることに注意が必要です。

## See Also (参照)
- Kotlin公式ドキュメント: [Basic Types](https://kotlinlang.org/docs/basic-types.html)
