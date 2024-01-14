---
title:                "Kotlin: 日付を文字列に変換する"
programming_language: "Kotlin"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ日付を文字列に変換するのか

日付を文字列に変換すると、より読みやすく、わかりやすい形式になります。また、データベースやAPIなど、さまざまなシステムとのやり取りに便利です。

## 日付を文字列に変換する方法

```Kotlin
val date = Date()
val dateFormat = SimpleDateFormat("yyyy/MM/dd")
val strDate = dateFormat.format(date)
println(strDate) // 出力：2021/06/09
```

上記の例では、まずDateオブジェクトを作成し、次にSimpleDateFormatクラスを使用して指定の形式の文字列に変換しています。必要に応じて、任意の形式に変更することができます。

## 日付を文字列に変換する際の詳細

日付を文字列に変換する際には、日付のパターンを指定する必要があります。パターンは、年、月、日などの要素がどのように並ぶかを指定するものです。また、Dateオブジェクトを作成する際には、指定したパターンに沿った文字列を渡す必要があります。

## この記事の他の関連情報

### 参考リンク

- [Kotlin Date and Time utility functions](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.time/java.util.-date/index.html)
- [Converting Date to String in Kotlin](https://www.javatpoint.com/kotlin-date-to-string)
- [How to Convert String to Date in Kotlin](https://www.tutorialkart.com/kotlin/date/how-to-convert-string-to-date-in-kotlin/)