---
title:                "未来または過去の日付を計算する。"
html_title:           "Java: 未来または過去の日付を計算する。"
simple_title:         "未来または過去の日付を計算する。"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何 & なぜ？
日付を将来または過去に計算することは、プログラマーにとって非常によく使われる機能です。これは、特定の日付から何かを計算することを可能にし、より柔軟なアプリケーションを作成することができるからです。
## 方法：
```java
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

public class CalculateDate {

    public static void main(String[] args) {

        // 現在の日付
        LocalDate today = LocalDate.now();
        System.out.println("現在の日付: " + today);

        // 1ヶ月後の日付を計算
        LocalDate oneMonthLater = today.plusMonths(1);
        System.out.println("1ヶ月後の日付: " + oneMonthLater);

        // 10年前の日付を計算
        LocalDate tenYearsAgo = today.minusYears(10);
        System.out.println("10年前の日付: " + tenYearsAgo);

        // 日付のフォーマットを指定
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy/MM/dd");

        // ある日付の特定のフォーマットで計算
        String birthday = "2000/01/01";
        LocalDate nextBirthday = LocalDate.parse(birthday, formatter).plusYears(1);
        System.out.println("来年の誕生日: " + nextBirthday.format(formatter));

    }
}
```
実行結果:
```
現在の日付: 2021-03-17
1ヶ月後の日付: 2021-04-17
10年前の日付: 2011-03-17
来年の誕生日: 2001/01/01
```
## 深堀り：
この日付の計算機能は、Java8で導入されたjava.timeパッケージに含まれています。以前のバージョンでは、DateやCalendarクラスを使用して日付計算を行っていましたが、その操作性や扱いにくさからjava.timeパッケージが導入されました。このパッケージはより直感的で使いやすいインターフェースを提供しています。また、Joda-TimeやThreeTen-Extraなどの代替ライブラリもあります。
## 関連記事：
- [Java8の日付と時刻API](https://docs.oracle.com/javase/jp/8/docs/api/java/time/package-summary.html)
- [Joda-Time](https://www.joda.org/joda-time/)
- [ThreeTen-Extra](https://www.threeten.org/threeten-extra/)