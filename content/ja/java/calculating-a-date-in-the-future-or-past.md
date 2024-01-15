---
title:                "将来または過去の日付計算"
html_title:           "Java: 将来または過去の日付計算"
simple_title:         "将来または過去の日付計算"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

日付を将来や過去に計算することの利点は多数あります。たとえば、旅行のプランニングや誕生日や記念日の計画を立てる際に、将来の日付を知ることは非常に役立ちます。また、レポートやプログラムで特定の日付を使う必要がある場合にも、日付を自動的に計算できることは非常に便利です。

## 使い方

日付を将来や過去に計算するには、JavaのDateやCalendarクラスを使用することができます。以下の例では、Dateクラスを使用して現在の日付を取得し、Calendarクラスを使用して指定した期間を加算する方法を示します。

```Java
// 現在の日付を取得
Date currentDate = new Date();

// Calendarオブジェクトを作成し、日付をセット
Calendar calendar = Calendar.getInstance();
calendar.setTime(currentDate);

// 1年後の日付を計算
calendar.add(Calendar.YEAR, 1);
Date futureDate = calendar.getTime();

// 1年後の日付を表示
System.out.println("1年後の日付は：" + futureDate);
```

上記のコードを実行すると、現在の日付から1年後の日付が計算されて表示されます。

## ディープダイブ

Javaでは、日付を計算するために様々なメソッドが提供されています。例えば、add()メソッドを使用すると、任意の期間を日付に加算することができます。また、get()メソッドを使用すると、日付から特定の情報（年、月、日など）を取得することができます。

さらに、Java 8からは、日付と時間を扱うための新しいAPIであるLocalDate、LocalTime、LocalDateTimeが追加されました。これらのクラスを使用すると、日付や時間をより柔軟に扱うことができます。

## はじめに参考にしたいページ

- [Java Dateクラスのドキュメント](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java Calendarクラスのドキュメント](https://docs.oracle.com/javase/8/docs/api/java/util/Calendar.html)
- [Java 8の新しい日付と時間API](https://www.oracle.com/technical-resources/articles/java/jf14-date-time.html)

## 関連記事をもっと読む

- [Javaで日付と時間を扱う際の基本的なこと](https://club.paragraph9.jp/entry/java-date-time-basics/)
- [Javaで日付の計算をスマートにするためのTips](https://www.baeldung.com/java-date-time-math)
- [Javaの日付と時間を扱う際の陥りがちなFallacies](https://blog.joda.org/2009/11/why-cant-i-just-add-one.html)