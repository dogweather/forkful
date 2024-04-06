---
date: 2024-01-20 17:35:20.131561-07:00
description: null
isCJKLanguage: true
lastmod: '2024-04-05T21:53:42.939086-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) \u6587\u5B57\u5217\u306E\u7D50\u5408\u306B\u306F\u6B74\u53F2\
  \u304C\u3042\u308A\u307E\u3059\u3002\u521D\u671F\u306E\u30D7\u30ED\u30B0\u30E9\u30DF\
  \u30F3\u30B0\u8A00\u8A9E\u3067\u306F\u64CD\u4F5C\u304C\u3088\u308A\u624B\u9593\u3067\
  \u3057\u305F\u304C\u3001\u73FE\u4EE3\u306E\u8A00\u8A9E\u306F\u7C21\u5358\u306B\u7D50\
  \u5408\u3092\u53EF\u80FD\u306B\u3057\u3066\u3044\u307E\u3059\u3002Kotlin\u3067\u306F\
  \u30D7\u30E9\u30B9\u6F14\u7B97\u5B50(`+`)\u3084\u6587\u5B57\u5217\u30C6\u30F3\u30D7\
  \u30EC\u30FC\u30C8\u3092\u4F7F\u3048\u308B\u305F\u3081\u76F4\u611F\u7684\u3067\u3059\
  \u3002\u5B9F\u88C5\u306E\u9762\u3067\u306F\u3001\u77ED\u3044\u6587\u5B57\u5217\u306E\
  \u7D50\u5408\u306F\u52B9\u7387\u304C\u826F\u3044\u3067\u3059\u304C\u3001\u591A\u304F\
  \u306E\u6587\u5B57\u5217\u3084\u5927\u304D\u306A\u30C7\u30FC\u30BF\u3092\u6271\u3046\
  \u5834\u5408\u306F`StringBuilder`\u3092\u4F7F\u3063\u305F\u307B\u3046\u304C\u6027\
  \u80FD\u304C\u826F\u304F\u306A\u308A\u307E\u3059\u3002Java\u306E`String`\u30AF\u30E9\
  \u30B9\u306F\u4E0D\u5909\u306A\u306E\u3067\u3001Kotlin\u3082\u540C\u3058\u30D0\u30C3\
  \u30AF\u30A8\u30F3\u30C9\u3092\u5229\u7528\u3059\u308B\u305F\u3081\u3001\u65B0\u3057\
  \u3044\u6587\u5B57\u5217\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u304C\u7D50\u5408\u306E\
  \u305F\u3073\u306B\u751F\u6210\u3055\u308C\u308B\u3053\u3068\u306B\u6CE8\u610F\u304C\
  \u5FC5\u8981\u3067\u3059\u3002"
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
