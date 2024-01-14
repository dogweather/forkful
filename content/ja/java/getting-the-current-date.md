---
title:                "Java: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

現在の日付を取得する理由は、プログラム内で日付を使用する必要があるからです。例えば、日付をファイル名やログメッセージに含める必要がある場合があります。また、タイムスタンプやアプリケーションの動作状況をトラッキングする際にも、現在の日付を取得する必要があります。

## 方法

Javaで現在の日付を取得するには、`Date`クラスを使用します。このクラスをインスタンス化すると、現在の日付と時間が自動的に格納されます。例えば、以下のようにコードを記述することで現在の日付を取得することができます。

```Java
Date date = new Date();
System.out.println(date);
```

上記のコードを実行すると、現在の日付と時間が以下のようにコンソールに出力されます。

`Tue Dec 01 12:00:00 JST 2020`

また、`SimpleDateFormat`クラスを使用することで、日付のフォーマットを指定することができます。例えば、以下のようにコードを記述することで、年月日のフォーマットで現在の日付を取得することができます。

```Java
Date date = new Date();
SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd");
System.out.println(format.format(date));
```

上記のコードを実行すると、現在の日付が`2020-12-01`のようにフォーマットされてコンソールに出力されます。

## ディープダイブ

`Date`クラスは、Javaの古いバージョンでは主に使用されていましたが、現在では推奨されている`LocalDate`クラスが存在します。`LocalDate`クラスは`Date`クラスよりも柔軟な操作が可能であり、さまざまなタイムゾーンに対応しています。また、`LocalDateTime`クラスを使用することで、日付と時間を同時に取得することもできます。

## 参考リンク

* [Dateクラスの公式ドキュメント](https://docs.oracle.com/javase/jp/8/docs/api/java/util/Date.html)
* [SimpleDateFormatクラスの公式ドキュメント](https://docs.oracle.com/javase/jp/8/docs/api/java/text/SimpleDateFormat.html)
* [LocalDateクラスの公式ドキュメント](https://docs.oracle.com/javase/jp/8/docs/api/java/time/LocalDate.html)
* [LocalDateTimeクラスの公式ドキュメント](https://docs.oracle.com/javase/jp/8/docs/api/java/time/LocalDateTime.html)

## 参考文献

[Javaで現在の日付を取得する方法](https://dev.classmethod.jp/articles/how-to-get-todays-date-in-java/)

[Understanding Date vs. LocalDate vs. LocalDateTime](https://www.baeldung.com/java-date-localdate-localdatetime)