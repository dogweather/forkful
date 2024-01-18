---
title:                "文字列から日付を解析する"
html_title:           "Kotlin: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何でやるの? 
日付を文字列から解析するとは、プログラマーが日付をコンピューターで処理するために、文字列から日付を取得することを意味します。例えば、ユーザーから入力された生年月日をデータとして受け取り、それをコードで処理するために、日付を文字列から解析する必要があります。

## やり方: 
Kotlinで文字列から日付を解析する方法を見ていきましょう。まず、文字列から日付を含むオブジェクトを作成し、それをパーサーに渡す必要があります。Kotlinでは、以下のように記述します。

```Kotlin
val dateString = "2021/06/22" // 解析する文字列
val dateFormat = SimpleDateFormat("yyyy/MM/dd") // 日付を含むオブジェクトを作成
val date = dateFormat.parse(dateString) // パーサーに文字列を渡して日付を取得
println(date) // 出力: Tue Jun 22 00:00:00 JST 2021
```

このようにして、文字列から日付を解析することができます。

## 深堀り: 
日付を文字列から解析する方法は、コンピューターが日付を処理する様々な方法の一つです。他のプログラミング言語やライブラリでも同様の機能を持つものがありますが、KotlinではJavaの標準ライブラリであるSimpleDateFormatを使用することで簡単に実装することができます。

日付の解析には、文字列から日付への変換だけでなく、日付のフォーマットの指定やバリデーションなども重要な要素です。そのため、Kotlinでは、より簡単に利用できる別のライブラリであるThreeTenを使用することもできます。

## See Also: 
- [Java SimpleDateFormat](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- [Kotlin ThreeTen](https://github.com/Kotlin-Standard-Library/ThreeTen)