---
aliases:
- /ja/kotlin/concatenating-strings/
date: 2024-01-20 17:35:20.131561-07:00
description: "\u6587\u5B57\u5217\u306E\u7D50\u5408\u3068\u306F\u3001\u8907\u6570\u306E\
  \u6587\u5B57\u5217\u3092\u4E00\u3064\u306B\u3059\u308B\u3053\u3068\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30B3\u30FC\u30C9\u306E\u60C5\u5831\u3092\
  \u7D44\u307F\u5408\u308F\u305B\u305F\u308A\u3001\u30E6\u30FC\u30B6\u30FC\u3078\u306E\
  \u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u6574\u5F62\u3059\u308B\u305F\u3081\u306B\u3053\
  \u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.871853
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u7D50\u5408\u3068\u306F\u3001\u8907\u6570\u306E\
  \u6587\u5B57\u5217\u3092\u4E00\u3064\u306B\u3059\u308B\u3053\u3068\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30B3\u30FC\u30C9\u306E\u60C5\u5831\u3092\
  \u7D44\u307F\u5408\u308F\u305B\u305F\u308A\u3001\u30E6\u30FC\u30B6\u30FC\u3078\u306E\
  \u30E1\u30C3\u30BB\u30FC\u30B8\u3092\u6574\u5F62\u3059\u308B\u305F\u3081\u306B\u3053\
  \u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9023\u7D50"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列の結合とは、複数の文字列を一つにすることです。プログラマーはコードの情報を組み合わせたり、ユーザーへのメッセージを整形するためにこれを行います。

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
