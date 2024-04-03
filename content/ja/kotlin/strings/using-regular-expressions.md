---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:28.991910-07:00
description: "\u3069\u306E\u3088\u3046\u306B\u4F7F\u3046\u304B: #."
lastmod: '2024-03-13T22:44:42.050312-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

## どのように使うか:


### 基本的なマッチング
Kotlinで文字列が特定のパターンと一致するかチェックするには、`Regex`クラスの`matches`メソッドを使用できます。

```kotlin
val pattern = "kotlin".toRegex()
val input = "I love kotlin"
val result = pattern.containsMatchIn(input)

println(result)  // 出力: true
```

### 文字列の部分を見つけて抽出する
パターンに一致する文字列の部分を見つけたい場合、Kotlinではすべてのマッチを反復処理できます：

```kotlin
val datePattern = "\\d{2}/\\d{2}/\\d{4}".toRegex()
val input = "今日の日付は07/09/2023です。"
val dates = datePattern.findAll(input)

for (date in dates) {
    println(date.value)
}
// 出力：07/09/2023
```

### テキストの置換
パターンに一致する文字列の部分を置換するのは、`replace`関数を使えば簡単です：

```kotlin
val input = "Username: user123"
val sanitizedInput = input.replace("\\d+".toRegex(), "XXX")

println(sanitizedInput)  // 出力：Username: userXXX
```

### 文字列の分割
regexパターンをデリミタとして使用して、文字列をリストに分割します：

```kotlin
val input = "1,2,3,4,5"
val numbers = input.split(",".toRegex())

println(numbers)  // 出力: [1, 2, 3, 4, 5]
```

### サードパーティのライブラリ：Kotest
[Kotest](https://github.com/kotest/kotest)は、Kotlinの組み込みのregexサポートを拡張する、人気のあるKotlinテストライブラリーです。特にテストケースでのバリデーションに便利です。

```kotlin
// Kotestがプロジェクトに追加されていると仮定
import io.kotest.matchers.string.shouldMatch

val input = "kotlin@test.com"
input shouldMatch "\\S+@\\S+\\.com".toRegex()

// これは、入力がメールパターンと一致する場合にテストを通過します。
```

Kotlinアプリケーションに正規表現を取り入れることで、洗練されたテキスト処理を効率的に実行できます。ユーザー入力のバリデーション、データの抽出、または文字列の変換を行う場合でも、regexパターンは強力なソリューションを提供します。
