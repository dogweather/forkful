---
title:                "2つの日付を比較する"
html_title:           "Elixir: 2つの日付を比較する"
simple_title:         "2つの日付を比較する"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何となぜ？

日付比較とは、2つの日付を比較することを指します。プログラマーはこれを使って、日付が同じであるか、特定の日付が別の日付より前か後かを確認します。

## 方法はこちら:

`java.time.LocalDate` クラスの `isBefore()` および `isAfter()` メソッドを使用することで、簡単に日付の比較が可能です。

```kotlin
import java.time.LocalDate

fun main() {
    val date1 = LocalDate.of(2020, 1, 15)
    val date2 = LocalDate.of(2021, 6, 20)

    println(date1.isBefore(date2)) // true
    println(date1.isAfter(date2)) // false
}
```
このコードは、 `date1` （2020年1月15日）が `date2`（2021年6月20日）より前であることを確認します。その結果、"true" と "false" が表示されます。

## 深堀り:

日付を比較する手法は多岐に渡りますが、 `java.time.LocalDate` クラスを用いる方法はJava 8 (2014年公開) から推奨されています。それ以前のJavaのバージョンでは、 `java.util.Date` または `java.util.Calendar` クラスが使われてきましたが、使いづらさと設計の問題から新しいAPIが推奨されるようになりました。

また、一部のコードでは `java.time.ZonedDateTime` を用いることで時間帯を考慮した厳密な日時比較を行います。ただし、日付のみを比較する場合は、 `LocalDate` クラスの利用が推奨されます。

## 関連リンク:

以下リンクも参考にしてください。

* [LocalDate (Oracle Java Documentation)](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
* [Java 8 Date (JavaPoint)](https://www.javatpoint.com/java-8-date)