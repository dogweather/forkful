---
title:                "文字列の連結"
aliases:
- /ja/kotlin/concatenating-strings/
date:                  2024-01-20T17:35:20.131561-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の連結"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/concatenating-strings.md"
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
