---
title:                "正規表現の使用"
aliases:
- ja/kotlin/using-regular-expressions.md
date:                  2024-02-03T19:17:28.991910-07:00
model:                 gpt-4-0125-preview
simple_title:         "正規表現の使用"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
