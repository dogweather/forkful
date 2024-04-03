---
date: 2024-01-26 03:40:48.513682-07:00
description: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\
  \u308B\u3068\u306F\u3001\u3042\u306A\u305F\u304C\u6271\u3063\u3066\u3044\u308B\u30C6\
  \u30AD\u30B9\u30C8\u30C7\u30FC\u30BF\u304B\u3089\u3001\u5358\u4E00\uFF08' '\uFF09\
  \u307E\u305F\u306F\u4E8C\u91CD\uFF08\"\u2026"
lastmod: '2024-03-13T22:44:42.047970-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\
  \u308B\u3068\u306F\u3001\u3042\u306A\u305F\u304C\u6271\u3063\u3066\u3044\u308B\u30C6\
  \u30AD\u30B9\u30C8\u30C7\u30FC\u30BF\u304B\u3089\u3001\u5358\u4E00\uFF08' '\uFF09\
  \u307E\u305F\u306F\u4E8C\u91CD\uFF08\" \"\uFF09\u306E\u5F15\u7528\u7B26\u306E\u30A4\
  \u30F3\u30B9\u30BF\u30F3\u30B9\u3092\u3059\u3079\u3066\u53D6\u308A\u9664\u304F\u3053\
  \u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\
  \u306F\u3001\u30C7\u30FC\u30BF\u306E\u30AF\u30EA\u30FC\u30CB\u30F3\u30B0\u3092\u884C\
  \u3046\u305F\u3081\u3001\u3055\u3089\u306B\u51E6\u7406\u3092\u884C\u3046\u305F\u3081\
  \u306E\u6E96\u5099\u3092\u3059\u308B\u305F\u3081\u3001\u307E\u305F\u306F\u5F15\u7528\
  \u7B26\u81EA\u4F53\u304C\u30C7\u30FC\u30BF\u306E\u610F\u5473\u306B\u95A2\u4FC2\u306A\
  \u3044\u3068\u304D\u306A\u3069\u306B\u3001\u3053\u308C\u3092\u884C\u3046\u5FC5\u8981\
  \u304C\u3088\u304F\u3042\u308A\u307E\u3059\u3002."
title: "\u6587\u5B57\u5217\u304B\u3089\u5F15\u7528\u7B26\u3092\u524A\u9664\u3059\u308B"
weight: 9
---

## 何となぜ？

文字列から引用符を削除するとは、あなたが扱っているテキストデータから、単一（' '）または二重（" "）の引用符のインスタンスをすべて取り除くことを意味します。プログラマーは、データのクリーニングを行うため、さらに処理を行うための準備をするため、または引用符自体がデータの意味に関係ないときなどに、これを行う必要がよくあります。

## 方法：

ここに、Kotlinで文字列から両方のタイプの引用符を削除する簡単な方法があります：

```kotlin
fun removeQuotes(input: String): String {
    return input.replace("\"", "").replace("'", "")
}

fun main() {
    val stringWithQuotes = "Kotlin \"rocks\" it's 'cool'"
    val stringWithoutQuotes = removeQuotes(stringWithQuotes)
    println(stringWithoutQuotes) // 出力: Kotlin rocks its cool
}
```

そして、一種類の引用符のみを削除したい場合は、他のreplace呼び出しをスキップします。

```kotlin
fun removeDoubleQuotes(input: String): String {
    return input.replace("\"", "")
}

fun removeSingleQuotes(input: String): String {
    return input.replace("'", "")
}

fun main() {
    val stringWithQuotes = "Kotlin \"rocks\" it's 'cool'"
    println(removeDoubleQuotes(stringWithQuotes)) // 出力: Kotlin rocks it's 'cool'
    println(removeSingleQuotes(stringWithQuotes)) // 出力: Kotlin "rocks" its cool
}
```

## より深く：

歴史的に、文字列の処理やエスケープ文字の扱いはプログラミングの中心的な部分であり、テキストはデータとインタフェースする基本的な方法です。文字列内の引用符は、時にエスケープする必要があります。これは先行するバックスラッシュ（例：`"She said, \"Hi!\""`）によって示されます。そのような文字列を処理する際には、エスケープ文字や引用符自体を取り除く必要があり、テキストをよりクリーンで使いやすくすることがあります。

`replace`メソッドに代わる手段には、正規表現に基づく削除や、文字ごとに手動で文字列を解析する方法が含まれます。しかし、正規表現は単純な操作には過剰であり、手動の解析は組み込みの文字列関数を使用するよりも効率が悪いです。Kotlinの`replace`関数は、性能に優れたJavaの`String` `replace`メソッドを活用しています。

実装に関しては、KotlinがJavaと相互運用可能であるため、文字列に対して行う操作はJavaである場合と同じくらいパフォーマンスが良いということに言及する価値があります。引用符を削除する際は、ネストした引用符などのエッジケースを意識することが重要であり、正規表現やパーサーライブラリを使用するより高度なアプローチが必要になる可能性があります。

## 参照：

Kotlinにおける文字列の扱いについて更に理解するためには、公式ドキュメントをご覧ください：

- [KotlinのStringドキュメント](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)

Kotlinでの正規表現と解析に関するより深いダイブについては：

- [Kotlin Regexドキュメント](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
