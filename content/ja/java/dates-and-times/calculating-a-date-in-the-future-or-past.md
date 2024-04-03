---
date: 2024-01-20 17:31:47.505554-07:00
description: "\u8A08\u7B97\u3059\u308B\u904E\u53BB\u3084\u672A\u6765\u306E\u65E5\u4ED8\
  \u3063\u3066\u4F55\uFF1F\u8981\u306F\u7279\u5B9A\u306E\u65E5\u304B\u3089\u6570\u3048\
  \u3066\u3069\u308C\u3060\u3051\u65E5\u304C\u7D4C\u3063\u305F\u304B\u3001\u307E\u305F\
  \u306F\u3069\u308C\u3060\u3051\u65E5\u304C\u6B8B\u3063\u3066\u3044\u308B\u304B\u3092\
  \u8A08\u7B97\u3059\u308B\u3053\u3068\u3060\u3002\u306A\u305C\u958B\u767A\u8005\u306F\
  \u3053\u308C\u3092\u3084\u308B\u306E\uFF1F\u30C7\u30C3\u30C9\u30E9\u30A4\u30F3\u7BA1\
  \u7406\u3001\u4E88\u7D04\u30B7\u30B9\u30C6\u30E0\u3001\u30EA\u30DE\u30A4\u30F3\u30C0\
  \u30FC\u6A5F\u80FD\u306A\u3069\u3067\u65E5\u4ED8\u8A08\u7B97\u306F\u4E0D\u53EF\u6B20\
  \u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.967205-06:00'
model: gpt-4-1106-preview
summary: "\u8A08\u7B97\u3059\u308B\u904E\u53BB\u3084\u672A\u6765\u306E\u65E5\u4ED8\
  \u3063\u3066\u4F55\uFF1F\u8981\u306F\u7279\u5B9A\u306E\u65E5\u304B\u3089\u6570\u3048\
  \u3066\u3069\u308C\u3060\u3051\u65E5\u304C\u7D4C\u3063\u305F\u304B\u3001\u307E\u305F\
  \u306F\u3069\u308C\u3060\u3051\u65E5\u304C\u6B8B\u3063\u3066\u3044\u308B\u304B\u3092\
  \u8A08\u7B97\u3059\u308B\u3053\u3068\u3060\u3002\u306A\u305C\u958B\u767A\u8005\u306F\
  \u3053\u308C\u3092\u3084\u308B\u306E\uFF1F\u30C7\u30C3\u30C9\u30E9\u30A4\u30F3\u7BA1\
  \u7406\u3001\u4E88\u7D04\u30B7\u30B9\u30C6\u30E0\u3001\u30EA\u30DE\u30A4\u30F3\u30C0\
  \u30FC\u6A5F\u80FD\u306A\u3069\u3067\u65E5\u4ED8\u8A08\u7B97\u306F\u4E0D\u53EF\u6B20\
  \u3002."
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
