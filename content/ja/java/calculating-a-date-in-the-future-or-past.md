---
title:                "将来または過去の日付を計算する"
html_title:           "Java: 将来または過去の日付を計算する"
simple_title:         "将来または過去の日付を計算する"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 何となぜ？

未来または過去の日付を計算するとは、特定の日付から特定の期間を加算または減算して新しい日付を得ることです。これにより、プログラマーは日付関連の複雑な問題を効率的に解決することができます。

# 方法：

Javaの標準ライブラリの'LocalDate'クラスを使って見てみましょう。

```Java
import java.time.LocalDate;
import java.time.Period;

public class FuturePastDate {
    public static void main(String[] args) {
        LocalDate today = LocalDate.now();
        Period p = Period.ofDays(10);    // Change the period as necessary

        LocalDate futureDate = today.plus(p);
        LocalDate pastDate = today.minus(p);

        System.out.println("Today's Date: " + today);
        System.out.println("Date after 10 days: " + futureDate);
        System.out.println("Date 10 days ago: " + pastDate);
    }
}
```

このコードを実行すると、以下の出力が得られます：

```
Today's Date: 2022-1-1
Date after 10 days: 2022-1-11
Date 10 days ago: 2021-12-22
```

# ディープダイブ:

(1) 歴史的背景:

Javaの'Date'クラスは、年-月-日の概念に基づく未来や過去の日付の計算に問題がありました。Java 8で導入された'LocalDate'と'Period'クラスはこの問題を解決しました。

(2) 別の方法:

Javaの'Calendar'クラスを使用して未来または過去の日付を計算する事もできますが、'LocalDate'クラスの使用が推奨されます。

(3) 実装詳細:

'LocalDate'クラスの'plus'と'minus'メソッドは、それぞれ未来と過去の日付を計算します。「Period」クラスは日付間の期間を定義します。

# 関連参考:

- Oracle公式Javaドキュメンテーション: https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html
- Oracle公式Javaチュートリアル-日付と時刻: https://docs.oracle.com/javase/tutorial/datetime/