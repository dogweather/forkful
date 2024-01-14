---
title:    "Java: 将来または過去の日付を計算する"
keywords: ["Java"]
---

{{< edit_this_page >}}

## なぜ
日付を未来や過去に計算することの重要性について説明します。日付を計算することで、予定を立てたり、イベントの日程を把握したりすることができます。

## 使い方
日付を計算する方法について、Javaのコーディング例と出力のサンプルをご紹介します。 ご自身のプロジェクトに応じて、この方法を使って日付を計算することができます。

```Java
// 現在の日付を取得
LocalDate today = LocalDate.now();

// 1日後の日付を計算
LocalDate date = today.plusDays(1);

// 1週間後の日付を計算
LocalDate date = today.plusWeeks(1);

// 1ヶ月後の日付を計算
LocalDate date = today.plusMonths(1);

// 1年後の日付を計算
LocalDate date = today.plusYears(1);

// 任意の日付から任意の期間を追加して計算
LocalDate date = LocalDate.of(2021, 1, 1).plus(Period.ofMonths(6)); // 2021年7月1日が出力される
```

## まるっと解説
Javaには、日付を計算するために便利なメソッドがいくつか用意されています。 `plusDays()`、 `plusWeeks()`、 `plusMonths()`、 `plusYears()`のメソッドを使うと、指定した日数、週数、月数、年数を現在の日付に加算した日付を取得することができます。また、 `plus()`メソッドを使うと、任意の期間を指定して日付を計算することができます。これらのメソッドを組み合わせることで、さまざまな日付の計算が可能になります。

## See Also
- [Java LocalDate documentation](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html)
- [Java Period documentation](https://docs.oracle.com/javase/8/docs/api/java/time/Period.html)