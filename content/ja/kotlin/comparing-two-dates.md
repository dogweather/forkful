---
title:                "2つの日付の比較"
html_title:           "Kotlin: 2つの日付の比較"
simple_title:         "2つの日付の比較"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

#Kotlinで日付の比較を行う方法

## 何となぜ？
日付の比較とは、２つの日付が同じかどうか、またどちらが大きいかを確認することです。プログラマーは、アプリケーションで日付を扱う際に、正しい日付を使用することが重要であるため、しばしば日付の比較を行います。

## 方法：
日付の比較は、Kotlinの組み込みのメソッドを使用して簡単に行うことができます。以下のコードを参考にしてください。

```Kotlin
val date1 = LocalDate.of(2020, 2, 14) //最初の日付
val date2 = LocalDate.of(2021, 5, 1) //2番目の日付

//２つの日付が同じかどうかを確認する
if (date1 == date2) {
    println("同じ日付です")
} else {
    println("違う日付です")
}

//どちらが大きいかを比較する
if (date1.isBefore(date2)) {
    println("最初の日付の方が前です")
} else if (date1.isAfter(date2)) {
    println("最初の日付の方が後です")
} else {
    println("同じ日付です")
}
```

上記のコードを実行すると、以下のような出力が得られます。

```
違う日付です
最初の日付の方が前です
```

## 詳細を知る：
日付の比較は、Java 8のリリースとともに、Java.timeパッケージで導入されました。それ以前は、比較メソッドを使用するために自分でコードを書く必要がありました。しかし、Kotlinでは組み込みのメソッドが用意されているため、より簡単に日付の比較を行うことができます。

代替手段としては、Joda-Timeライブラリを使用する方法があります。Joda-Timeは、Java 8のリリースされる前に利用可能でしたが、KotlinではJava.timeと同じように利用できます。

## 参考リンク：
- [Kotlinの日付と時刻の操作(Methods for manipulating dates and times in Kotlin)](https://www.geeksforgeeks.org/methods-manipulating-dates-times-kotlin/)
- [Java 8日付/時刻パッケージ(Tutorial: Java 8 Date/Time API)](https://www.baeldung.com/java-8-date-time-intro)