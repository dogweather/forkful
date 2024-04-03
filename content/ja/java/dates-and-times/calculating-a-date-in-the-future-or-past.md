---
date: 2024-01-20 17:31:47.505554-07:00
description: "How to: (\u3084\u308A\u65B9) Java\u3067\u672A\u6765\u306E\u65E5\u4ED8\
  \u3092\u8A08\u7B97\u3059\u308B\u306E\u306F\u7C21\u5358\u3002`LocalDate`\u3068`Period`\u30AF\
  \u30E9\u30B9\u3092\u4F7F\u304A\u3046\u3002\u30D1\u30B9\u30C8\u65E5\u4ED8\u3082\u4F3C\
  \u305F\u308A\u5BC4\u3063\u305F\u308A\u3002\u898B\u3066\u307F\u3088\u3046\uFF1A."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.967205-06:00'
model: gpt-4-1106-preview
summary: "Java\u3067\u672A\u6765\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\u3059\u308B\u306E\
  \u306F\u7C21\u5358\u3002`LocalDate`\u3068`Period`\u30AF\u30E9\u30B9\u3092\u4F7F\u304A\
  \u3046\u3002\u30D1\u30B9\u30C8\u65E5\u4ED8\u3082\u4F3C\u305F\u308A\u5BC4\u3063\u305F\
  \u308A\u3002\u898B\u3066\u307F\u3088\u3046\uFF1A."
title: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\
  \u3059\u308B"
weight: 26
---

## How to: (やり方)
Javaで未来の日付を計算するのは簡単。`LocalDate`と`Period`クラスを使おう。パスト日付も似たり寄ったり。見てみよう：

```java
import java.time.LocalDate;
import java.time.Period;

public class DateCalculator {
    public static void main(String[] args) {
        LocalDate now = LocalDate.now();

        // 未来の日付を計算
        LocalDate futureDate = now.plus(Period.ofDays(10));
        System.out.println("10日後の日付: " + futureDate);

        // 過去の日付を計算
        LocalDate pastDate = now.minus(Period.ofDays(10));
        System.out.println("10日前の日付: " + pastDate);
    }
}
```

サンプル出力:

```
10日後の日付: 2023-04-21
10日前の日付: 2023-03-31
```

`now.plus()`と`now.minus()`を使って日付の加算、減算ができる。期間は`Period`クラスで定義。

## Deep Dive (掘り下げ)
Javaでの日付操作はずっと進化している。`java.util.Date`が始まりだったけど、問題点多数。Java 8以降では`java.time`パッケージが導入され、`LocalDate`、`LocalTime`、`LocalDateTime`など、使いやすく正確な日付処理クラスが提供されている。

選択肢は他にもある。Joda-Timeライブラリなんかが前は人気だったけど、今は`java.time`の方がスタンダード。便利なので外部ライブラリに頼る必要は減った。

実装の詳細に入ると、タイムゾーンの考慮が重要になることがある。`ZonedDateTime`クラスでタイムゾーン付きの日付時刻を扱える。また、日付の差分を計算する際は`Duration`クラスを使うと時間や分単位での精密な計算が可能。

## See Also (関連情報)
- [Java Date and Time API Guide](https://www.oracle.com/java/technologies/javase/jdk8-jre8-suported-locales.html)
- [Java Documentation for java.time](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Joda-Time](https://www.joda.org/joda-time/) (旧式だが参考にはなる)

Javaと日付操作は深い関係にある。上記リンクでさらに学べる。コードを書いてみて、日付計算のプロになろう。
