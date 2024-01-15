---
title:                "日付を文字列に変換する"
html_title:           "Kotlin: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換することのメリットは、データや情報を扱う上で重要な要素です。日付をわかりやすい形式で表現することで、データの管理や表示が容易になります。

## 方法

日付を文字列に変換する方法について、Kotlinのコーディング例と出力例を示します。

```Kotlin
val currentDate = Date() // 現在の日付を取得
val stringDate = SimpleDateFormat("dd/MM/yyyy").format(currentDate) // 日付を指定した形式の文字列に変換
println(stringDate) // 出力例：07/09/2021
```

さらに、特定の地域や言語に合わせた形式で日付を文字列化することも可能です。具体的なコード例は以下の通りです。

```Kotlin
val japanLocale = Locale("ja", "JP") // 日本語の地域と言語を指定
val currentDate = Date() // 現在の日付を取得
val stringDate = SimpleDateFormat("yyyy年MM月dd日", japanLocale).format(currentDate) // 日付を日本語形式に変換
println(stringDate) // 出力例：2021年09月07日
```

## 深堀り

日付を文字列に変換する際には、SimpleDateFormatクラスを使用します。このクラスは、日付を指定した書式に従って文字列に変換する機能を持っています。また、Localeクラスを使用することで、地域や言語に応じた形式で日付を文字列化することができます。

## 関連リンク

- [Java SimpleDateFormat Class](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html)
- [Locale Class in Java](https://docs.oracle.com/javase/7/docs/api/java/util/Locale.html)
- [Kotlin Date and Time API](https://kotlinlang.org/docs/datetime.html)