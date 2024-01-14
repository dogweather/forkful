---
title:    "Java: 「現在の日付を取得する」"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/java/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ日付を取得するのか

日付を取得することは、多くのプログラミング言語において非常に一般的な作業です。日付に関するデータや情報を扱う場合、通常は最新の情報を使いたいと思うでしょう。そのため、プログラムで現在の日付を取得する必要が生じる場面がしばしばあります。Javaでは、現在の日付を取得する方法がさまざまな方法で用意されています。ここでは、Javaで現在の日付を取得する方法について詳しく見ていきましょう。

## 取得方法

Javaには、あらかじめ用意されたクラスライブラリを介して現在の日付を取得する方法があります。例えば、`LocalDate`クラスを使用することで現在の日付を取得することができます。下のコードブロックでは、`LocalDate`クラスを使用して現在の日付を取得する方法を示します。

```java
// 現在の日付を取得する
LocalDate date = LocalDate.now();

// 現在の日付を出力する
System.out.println("現在の日付は：" + date);
```

出力結果は以下のようになります。

```
現在の日付は：2021-12-09
```

また、より詳細な情報を取得するには、`LocalDate`クラスのメソッドを使用することもできます。例えば、`getYear()`や`getMonth()`、`getDayOfMonth()`メソッドを使用することで、年や月、日の情報を個別に取得できます。

```java
// 年を取得する
int year = date.getYear();

// 月を取得する
Month month = date.getMonth();

// 日を取得する
int day = date.getDayOfMonth();

// 年月日を出力する
System.out.println("今日は" + year + "年" + month + "月" + day + "日です。");
```

出力結果は以下のようになります。

```
今日は2021年12月9日です。
```

他にも`LocalDateTime`クラスや`Calendar`クラスを使用して日付を取得することができます。しかし、この記事では詳細には触れませんので、興味があればそれぞれのクラスについて調べてみてください。

## 深堀り

Javaには、単に現在の日付を取得するだけではなく、さまざまな方法で日付を操作することができる機能も用意されています。例えば、`LocalDate`クラスには、指定した日付の前後の日付を取得する`plusDays()`や`minusDays()`メソッドがあります。

```java
// 明日の日付を取得する
LocalDate tomorrow = date.plusDays(1);

// 明後日の日付を取得する
LocalDate dayAfterTomorrow = date.plusDays(2);

// 昨日の日付を取得する
LocalDate yesterday = date.minusDays(1);
```

他にも、指定した日付が何曜日かを取得したり、指定した日付が何週目かを取得するメソッドもあります。詳しくは公式ドキュメントを参考にしてください。

## See Also

- [LocalDateクラス | Javaドキュメント](https://docs.oracle.com/javase/jp/8/docs/api/java/time/LocalDate.html)
- [LocalDateTimeクラス | Javaドキュメント](https://docs.oracle.com/javase/jp/8/docs/api/java/time/LocalDateTime.html)
-