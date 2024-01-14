---
title:                "Elixir: 将来または過去の日付を計算する"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ
今日はElixirで将来や過去の日付を計算することについて説明します。私たちは、生活の中で日付を計算する必要があります。例えば、予定を立てたり、期限を追加したり、誕生日を計算したりします。Elixirを使えば、このような日付計算を簡単に実装することができます。

## 方法
まず、`Date`モジュールを使って現在の日付を取得します。例えば、`Date.utc_today()`は現在の日付をUTCで取得することができます。次に、`Date.add()`関数を使って、現在の日付から指定した日数を足したり引いたりすることができます。例えば、`Date.add(Date.utc_today(), 7)`は現在の日付から7日後の日付を計算します。

```Elixir
current_date = Date.utc_today()
future_date = Date.add(current_date, 7) # 今日の日付から7日後の日付を計算
past_date = Date.add(current_date, -7) # 今日の日付から7日前の日付を計算
```

このようにして、現在の日付から任意の日数を加算または減算することで、将来や過去の日付を簡単に計算することができます。

## 深堀り
さらに興味を持っている方には、`Calendar`モジュールを使ってさまざまな日付計算を行うことができます。例えば、`Calendar.DateTime`を使うことで、特定の時刻を含む日付を計算することができます。また、`Calendar.Date`を使うことで、特定の日付を含む週や月を計算することができます。

また、`Calendar`モジュールには多くの便利な関数が用意されていますので、ぜひ使ってみてください。

## 参考リンク
- [Elixir Dateモジュールの公式ドキュメント（英語）](https://hexdocs.pm/elixir/master/Date.html)
- [Elixir Calendarモジュールの公式ドキュメント（英語）](https://hexdocs.pm/elixir/master/Calendar.html)