---
title:                "Kotlin: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換する理由はさまざまです。例えば、データベースに保存された日付をユーザーが読みやすい形式で表示するためや、日付を特定の形式でファイル名として使用するためなどです。

## 方法

```kotlin
// 元の日付を作成
val date = LocalDate.of(2021, 10, 31)

// 日付を文字列に変換
val formattedDate = date.format(DateTimeFormatter.ofPattern("yyyy-MM-dd"))

// 出力：2021-10-31
println(formattedDate)
```

日付を文字列に変換するには、まず`LocalDate`クラスを使用して日付を作成します。そして、`DateTimeFormatter`クラスを使用して任意の形式の文字列に変換します。`ofPattern`メソッドによってフォーマットを指定することができます。上記の例では、`yyyy-MM-dd`という形式で日付を表しています。

## 深堀り

日付を文字列に変換する際、パターン文字列には様々なオプションがあります。例えば、`yy`を使用すると年を2桁で表すことができます。また、`MMM`を使用すると月を3文字の英語表記で表示することができます。詳しくは公式ドキュメントを参照してみてください。

## 参考リンク

- [Java 8日付と時間の新しいAPI](https://docs.oracle.com/javase/jp/8/docs/api/java/time/package-summary.html)
- [DateTimeFormatterクラスのドキュメント](https://docs.oracle.com/javase/jp/8/docs/api/java/time/format/DateTimeFormatter.html)
- [LocalDateクラスのドキュメント](https://docs.oracle.com/javase/jp/8/docs/api/java/time/LocalDate.html)

## 参考に

- [kotlin-japanese-translation-ja](https://github.com/kotlin-japanese-translation)