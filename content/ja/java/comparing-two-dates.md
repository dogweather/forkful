---
title:                "Java: 二つの日付の比較"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

日常生活では、日付を比較したり、特定の日付から経過時間を計算したりすることがよくあります。このようなタスクを効率的に実行するために、Javaプログラミングで日付を比較する方法を学ぶことは非常に重要です。

## 方法

日付を比較するための基本的な方法は、`compareTo()`メソッドを使用することです。このメソッドは2つの日付を比較し、等しい場合には0、第一引数の日付がより前の場合には負の数、第二引数の日付がより後ろの場合には正の数を返します。

```Java
Date date1 = new Date(2020, 9, 1);
Date date2 = new Date(2021, 1, 1);
Integer result = date1.compareTo(date2);
System.out.println(result); 
// output: -1 (date1 is before date2)
```

また、ミリ秒単位で比較する方法もあります。2つの日付をミリ秒単位で取得し、それらの差を比較することで行います。

```Java
Date date1 = new Date(2020, 9, 1, 12, 0, 0);
Date date2 = new Date(2020, 9, 1, 12, 0, 30);
Long diff = date2.getTime() - date1.getTime();
// diff is 30,000 milliseconds (30 seconds)
```

## ディープダイブ

日付を比較するためのさらに高度な方法として、Java 8から導入された`LocalDate`クラスを使用する方法があります。これを使用すると、2つの日付の日付部分のみを比較することができます。

```Java
LocalDate date1 = LocalDate.of(2020, 9, 1);
LocalDate date2 = LocalDate.of(2021, 1, 1);
Integer result = date1.compareTo(date2);
System.out.println(result); 
// output: -1 (date1 is before date2)
```

また、`Period`クラスを使用することで、日付間の経過時間を簡単に計算することもできます。

```Java
Period period = Period.between(date1, date2);
System.out.println(period.getMonths() + " months and " + period.getDays() + " days");
// output: 4 months and 0 days
```

## 参考リンク

- [Java Date and Time API Overview](https://www.baeldung.com/java-8-date-time-intro)
- [Java 8 LocalDate](https://www.baeldung.com/java-8-date-time-intro#java-8-localdate)
- [Java 8 Period](https://www.baeldung.com/java-8-date-time-intro#java-8-period)
- [Java Dateクラスのドキュメント](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)
- [Java LocalDateクラスのドキュメント](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)


## 参考文献

- [Javaで日付を比較する方法](https://www.baeldung.com/java-date-compare)