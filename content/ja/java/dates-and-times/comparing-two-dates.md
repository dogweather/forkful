---
date: 2024-01-20 17:33:26.441953-07:00
description: "Java\u3067\u300C2\u3064\u306E\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B\
  \u300D\u3068\u306F\u3001\u5358\u7D14\u306B2\u3064\u306E\u65E5\u4ED8\u304C\u540C\u3058\
  \u304B\u3001\u3069\u3061\u3089\u304C\u5148\u304B\u5F8C\u304B\u3092\u8ABF\u3079\u308B\
  \u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u6709\u52B9\
  \u671F\u9650\u306E\u30C1\u30A7\u30C3\u30AF\u3001\u30A4\u30D9\u30F3\u30C8\u306E\u30B9\
  \u30B1\u30B8\u30E5\u30FC\u30EB\u8ABF\u6574\u3001\u30C7\u30FC\u30BF\u30BD\u30FC\u30C8\
  \u306A\u3069\u69D8\u3005\u306A\u7406\u7531\u3067\u65E5\u4ED8\u3092\u6BD4\u8F03\u3057\
  \u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.965639-06:00'
model: gpt-4-1106-preview
summary: "Java\u3067\u300C2\u3064\u306E\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B\u300D\
  \u3068\u306F\u3001\u5358\u7D14\u306B2\u3064\u306E\u65E5\u4ED8\u304C\u540C\u3058\u304B\
  \u3001\u3069\u3061\u3089\u304C\u5148\u304B\u5F8C\u304B\u3092\u8ABF\u3079\u308B\u3053\
  \u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u6709\u52B9\u671F\
  \u9650\u306E\u30C1\u30A7\u30C3\u30AF\u3001\u30A4\u30D9\u30F3\u30C8\u306E\u30B9\u30B1\
  \u30B8\u30E5\u30FC\u30EB\u8ABF\u6574\u3001\u30C7\u30FC\u30BF\u30BD\u30FC\u30C8\u306A\
  \u3069\u69D8\u3005\u306A\u7406\u7531\u3067\u65E5\u4ED8\u3092\u6BD4\u8F03\u3057\u307E\
  \u3059\u3002"
title: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となんで？)

Javaで「2つの日付を比較する」とは、単純に2つの日付が同じか、どちらが先か後かを調べることです。プログラマーは有効期限のチェック、イベントのスケジュール調整、データソートなど様々な理由で日付を比較します。

## How to: (やり方)

```java
import java.time.LocalDate;

public class DateComparison {
    public static void main(String[] args) {
        LocalDate date1 = LocalDate.of(2023, 4, 1);
        LocalDate date2 = LocalDate.now();

        if (date1.isAfter(date2)) {
            System.out.println("date1 は date2 より後の日付です。");
        } else if (date1.isBefore(date2)) {
            System.out.println("date1 は date2 より前の日付です。");
        } else {
            System.out.println("date1 と date2 は同じ日付です。");
        }
    }
}
```

サンプル出力:
```
date1 は date2 より前の日付です。
```

## Deep Dive (掘り下げ)

日付の比較はJavaの初期バージョンから可能でしたが、Java 8で導入されたLocalDateクラスによってさらに簡単になりました。LocalDateは時刻情報を含まず、日付だけを扱うイミュータブルなクラスです。

他の代替手段としては、CalendarクラスやDateクラスを使う方法がありますが、これらは使うのが面倒でエラーが発生しやすいため、現在はあまり推奨されていません。

実装の詳細としては、LocalDateはChronoLocalDateインターフェースを実装しており、比較にはComparableインターフェースのcompareToメソッドを利用します。この方法は内部で日付を数値化し比較するため、直感的かつ高速です。

## See Also (参照)

- [LocalDate (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java Date and Time (Tutorialspoint)](https://www.tutorialspoint.com/java8/java8_datetime_api.htm)
- [ChronoLocalDate (Java Platform SE 8)](https://docs.oracle.com/javase/8/docs/api/java/time/chrono/ChronoLocalDate.html)
