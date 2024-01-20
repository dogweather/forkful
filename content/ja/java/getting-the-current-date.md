---
title:                "現在の日付を取得する"
date:                  2024-01-20T15:15:18.377456-07:00
html_title:           "Bash: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
プログラムで「現在の日付」を取得するってどういうこと？メインは、今日の日付や時間を知る方法だ。なぜこれが必要か？ログ記録、タイムスタンプやタイムゾーンの管理など、様々なケースで重要になる。

## How to: (やり方)
Javaで現在の日付を取得する基本的なコード：

```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class GetCurrentDate {
    public static void main(String[] args) {
        LocalDate today = LocalDate.now();
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy/MM/dd");
        String formattedDate = today.format(formatter);
        
        System.out.println(formattedDate);  // 出力: 2023/01/30 (例)
    }
}
```

## Deep Dive (深掘り)
昔々、`java.util.Date`が日付操作の全てだった。でも、このクラスは扱いづらいと評判だった。2014年、Java 8が`java.time`パッケージを導入し、日付と時間の作業を劇的に改善した。この新しいAPIは不変(Immutable)で、スレッドセーフだ。`LocalDate`、`LocalTime`、`LocalDateTime`クラスが主役だ。

代替として、古い`java.util.Calendar`を使うこともできるけど、新しいAPIを使うのが今はスタンダード。タイムゾーンを扱う必要がある場合は、`ZonedDateTime`や`OffsetDateTime`を使おう。

実装の詳細にオリエントするなら、`DateTimeFormatter`を使って日付を任意の形式に変換できる。それに加え、`java.time`パッケージはテストを容易にするためのクロックの概念を提供する。

## See Also (参照)
- [Oracle's Java Tutorials on Date Time](https://docs.oracle.com/javase/tutorial/datetime/index.html)
- [Java 8 Date/Time API Guide by Baeldung](https://www.baeldung.com/java-8-date-time-intro)
- [`java.time` package JavaDoc](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)