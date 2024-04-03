---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:08.119414-07:00
description: "\u65B9\u6CD5\uFF1A Kotlin\u3067\u306F\u3001\u6A19\u6E96\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u95A2\u6570\u3092\u4F7F\u7528\u3057\u3066\u6587\u5B57\u5217\u3092\u5927\
  \u6587\u5B57\u306B\u3059\u308B\u3053\u3068\u304C\u3067\u304D\u3001\u30B5\u30FC\u30C9\
  \u30D1\u30FC\u30C6\u30A3\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u306F\u5FC5\u8981\u3042\
  \u308A\u307E\u305B\u3093\u3002Kotlin\u306E\u6587\u5B57\u5217\u51E6\u7406\u65B9\u6CD5\
  \u306B\u3088\u308A\u3001\u3053\u308C\u3089\u306E\u64CD\u4F5C\u306F\u7C21\u6F54\u3067\
  \u308F\u304B\u308A\u3084\u3059\u304F\u306A\u3063\u3066\u3044\u307E\u3059\u3002 #."
lastmod: '2024-03-13T22:44:42.041160-06:00'
model: gpt-4-0125-preview
summary: "Kotlin\u3067\u306F\u3001\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u95A2\
  \u6570\u3092\u4F7F\u7528\u3057\u3066\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\
  \u3059\u308B\u3053\u3068\u304C\u3067\u304D\u3001\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\
  \u30A3\u306E\u30E9\u30A4\u30D6\u30E9\u30EA\u306F\u5FC5\u8981\u3042\u308A\u307E\u305B\
  \u3093\u3002Kotlin\u306E\u6587\u5B57\u5217\u51E6\u7406\u65B9\u6CD5\u306B\u3088\u308A\
  \u3001\u3053\u308C\u3089\u306E\u64CD\u4F5C\u306F\u7C21\u6F54\u3067\u308F\u304B\u308A\
  \u3084\u3059\u304F\u306A\u3063\u3066\u3044\u307E\u3059."
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
weight: 2
---

## 方法：
Kotlinでは、標準ライブラリ関数を使用して文字列を大文字にすることができ、サードパーティのライブラリは必要ありません。Kotlinの文字列処理方法により、これらの操作は簡潔でわかりやすくなっています。

### 文字列全体を大文字化する：
```kotlin
val message = "hello, world!"
val capitalizedMessage = message.uppercase()

println(capitalizedMessage) // 出力: HELLO, WORLD!
```

### 最初の文字のみを大文字化する：
Kotlin 1.5以降、`capitalize()`関数は非推奨とされ、`replaceFirstChar`と、それが小文字であれば大文字に変換するラムダを組み合わせて置き換えられました。

```kotlin
val greeting = "hello, world!"
val capitalizedGreeting = greeting.replaceFirstChar {
    if (it.isLowerCase()) it.titlecase() else it.toString()
}

println(capitalizedGreeting) // 出力: Hello, world!
```

この方法では、文の残りの部分をそのまま維持しつつ、最初の文字のみを大文字に変更します。
