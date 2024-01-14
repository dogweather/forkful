---
title:                "Kotlin: 「2つの日付の比較」"
simple_title:         "「2つの日付の比較」"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングをするとき、時々二つの日付を比較する必要があります。それは、アプリケーションで重要な日付を追跡したい場合や、ユーザーにとって忘れられないイベントをリマインダーするための機能を作成する場合などに役立ちます。

## 使い方

Kotlinの```Date```クラスは、2つの日付を比較するための便利なメソッドを持っています。まず、比較する二つの日付を```Date```オブジェクトに変換します。

```Kotlin
val date1 = Date("2021-03-15")
val date2 = Date("2021-03-20")
```

次に、```before()```や```after()```などのメソッドを使って比較を行います。

```Kotlin
println(date1.after(date2)) // Output: false
println(date1.before(date2)) // Output: true
```

また、```compareTo()```メソッドを使っても比較できます。このメソッドは、比較対象の日付が等しい場合は0を返し、比較対象よりも前の場合は負の値を、比較対象よりも後ろの場合は正の値を返します。

```Kotlin
println(date1.compareTo(date2)) // Output: -1
```

## 深堀り

日付の比較にはさまざまな方法がありますが、```Date```クラスのメソッドを使うことで簡単に比較できます。また、```LocalDate```や```Calendar```などのクラスを使うこともできますが、それぞれに特有のメソッドがあるので、使い方をしっかりと把握する必要があります。

さらに、日付の比較はタイプセーフな方法で行うことが重要です。Kotlinでは、```LocalDate```や```ZonedDateTime```などのJava 8で導入されたクラスを使うことで、タイプセーフな日付の比較が可能となっています。

## 参考リンク

- [KotlinのDateクラスのドキュメント](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-date/)
- [Java 8で導入された日付/時刻APIのドキュメント](https://docs.oracle.com/javase/jp/8/docs/api/java/time/package-summary.html)
- [プログラマーのための日付の扱い方 全部くわしく解説！](https://programmingapplications.net/ja/date-comparison/)