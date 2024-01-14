---
title:                "Java: 「未来または過去の日付を計算する」"
programming_language: "Java"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why
日本語訳： なぜ今、未来や過去の日付を計算する必要があるのでしょうか？

## How To
日本語訳：```Java ... ```コードブロック内のコーディング例とサンプル出力。

未来の日付を計算する場合、```LocalDate```クラスを使用して以下のように実装することができます。

```Java
// 今日の日付を取得
LocalDate today = LocalDate.now();

// 5日後の日付を計算
LocalDate fiveDaysLater = today.plusDays(5);

// 結果を出力
System.out.println("5 days later: " + fiveDaysLater);
```

これにより、今日の日付から5日後の日付が計算され、コンソール上に表示されます。

過去の日付を計算する場合も同様に、```minusDays()```メソッドを使用して計算することができます。

```Java
// 今日の日付を取得
LocalDate today = LocalDate.now();

// 5日前の日付を計算
LocalDate fiveDaysAgo = today.minusDays(5);

// 結果を出力
System.out.println("5 days ago: " + fiveDaysAgo);
```

## Deep Dive
日本語訳：未来や過去の日付を計算するためには、Java 8以降で導入された```java.time```パッケージを使用することが推奨されます。このパッケージには、日付や時間を管理するための便利なクラスやメソッドが含まれており、少ないコード量で簡単に日付計算を行うことができます。

また、日付を計算する際には、タイムゾーンやロケールの設定にも注意が必要です。これらの設定を正しく行わないと、意図しない結果が得られる可能性があります。

## See Also
日本語訳：参考リンク

- [Java 8で導入されたjava.timeパッケージの公式ドキュメント](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Javaの日付操作に関するチートシート](https://www.baeldung.com/java-date-time)
- [タイムゾーンとロケールの設定についての説明](https://stackify.com/preparing-for-multi-region-deployments-java-time-zone/)