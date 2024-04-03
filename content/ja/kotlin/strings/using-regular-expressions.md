---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:28.991910-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.050312-06:00'
model: gpt-4-0125-preview
summary: "\u6B63\u898F\u8868\u73FE\uFF08regex\uFF09\u306F\u3001\u30C6\u30AD\u30B9\u30C8\
  \u51E6\u7406\u306B\u304A\u3044\u3066\u5F37\u529B\u306A\u30C4\u30FC\u30EB\u3067\u3042\
  \u308A\u3001\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u304C\u9AD8\u5EA6\u306A\u30D1\u30BF\
  \u30FC\u30F3\u30DE\u30C3\u30C1\u30F3\u30B0\u6280\u8853\u3092\u4F7F\u7528\u3057\u3066\
  \u6587\u5B57\u5217\u306E\u691C\u7D22\u3001\u30DE\u30C3\u30C1\u30F3\u30B0\u3001\u304A\
  \u3088\u3073\u64CD\u4F5C\u3092\u884C\u3048\u308B\u3088\u3046\u306B\u3057\u307E\u3059\
  \u3002Kotlin\u3067\u306F\u3001regex\u3092\u6D3B\u7528\u3059\u308B\u3053\u3068\u3067\
  \u3001\u30D0\u30EA\u30C7\u30FC\u30B7\u30E7\u30F3\u3001\u30D1\u30FC\u30B9\u3001\u307E\
  \u305F\u306F\u5909\u63DB\u306E\u3088\u3046\u306A\u8907\u96D1\u306A\u30C6\u30AD\u30B9\
  \u30C8\u51E6\u7406\u30BF\u30B9\u30AF\u3092\u52B9\u7387\u7684\u306B\u5B9F\u884C\u3067\
  \u304D\u3001\u30B7\u30F3\u30D7\u30EB\u306A\u6587\u5B57\u5217\u64CD\u4F5C\u304B\u3089\
  \u8907\u96D1\u306A\u30C6\u30AD\u30B9\u30C8\u5206\u6790\u306B\u81F3\u308B\u307E\u3067\
  \u306E\u30BF\u30B9\u30AF\u306B\u3068\u3063\u3066\u4E0D\u53EF\u6B20\u3067\u3059\u3002\
  ."
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

## 何となく?

正規表現（regex）は、テキスト処理において強力なツールであり、プログラマーが高度なパターンマッチング技術を使用して文字列の検索、マッチング、および操作を行えるようにします。Kotlinでは、regexを活用することで、バリデーション、パース、または変換のような複雑なテキスト処理タスクを効率的に実行でき、シンプルな文字列操作から複雑なテキスト分析に至るまでのタスクにとって不可欠です。

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
