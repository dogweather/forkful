---
title:                "二つの日付の比較"
html_title:           "Java: 二つの日付の比較"
simple_title:         "二つの日付の比較"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ
どのように、どのようにして、javaプログラムを使用して二つの日付を比較するかを学ぶ必要があるかをお考えだと思います。ここでは、二つの日付を比較するJavaプログラムを作成する方法をご紹介します。

## 使い方
日付を比較するには、Javaには多くの方法がありますが、ここでは二つの日付を比較する方法を「LocalDate」クラスを使用して説明します。

```
Java
// 2つの日付を定義する
LocalDate date1 = LocalDate.of(2021, 1, 1);
LocalDate date2 = LocalDate.of(2020, 12, 31);

// 比較する
if (date1.isAfter(date2)) {
    System.out.println("Date 1 is after Date 2");
} else if (date1.isBefore(date2)) {
    System.out.println("Date 1 is before Date 2");
} else {
    System.out.println("Date 1 is equal to Date 2");
}

// 出力結果: Date 1 is after Date 2
```

また、日付を文字列から作成して比較することもできます。

```
// 文字列から日付を作成
DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy/MM/dd");
LocalDate date1 = LocalDate.parse("2021/01/01", formatter);
LocalDate date2 = LocalDate.parse("2020/12/31", formatter);

// 比較する
if (date1.isEqual(date2)) {
    System.out.println("Date 1 is equal to Date 2");
} else {
    System.out.println("Date 1 is not equal to Date 2");
}

// 出力結果: Date 1 is not equal to Date 2
```

## 深堀り
Javaでは、二つの日付を比較するために、`equals()`メソッドや `compareTo()`メソッドを使用することもできます。しかし、`LocalDate`クラスの `isBefore()`や `isAfter()`、`isBefore()`メソッドを使用することで、より直感的な方法で比較することができます。

さらに、Java 8以降では、新しく導入された`Period`クラスを使用することで、二つの日付の差を計算することができます。このクラスを使用することで、日付の表記を変えた場合でも正確な差を計算することができます。

## 関連記事を見る
- [Javaで日付を比較する方法](https://code.i-harness.com/ja/q/176ae36)
- [Java8の新しい日付と時刻API](https://codezine.jp/article/detail/8111)
- [Javaの日付を操作する方法](https://java-code.jp/165)