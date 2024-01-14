---
title:    "Elixir: 未来または過去の日付の計算"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

日付を未来や過去に計算することは、Elixirプログラミングでよく行われるタスクの一つです。例えば、日付を操作してイベントのスケジュールを作成したり、休日を計算したりすることができます。

## 方法

日付を未来や過去に計算するために、Elixirには2つの主要な関数があります。一つは `Date.add/2` で、もう一つは `Date.subtract/2` です。この記事では、それぞれの関数の使い方をコード例と共に示します。

```Elixir
# 今日から10日後の日付を計算する
today = Date.today()
future_date = Date.add(today, 10)
IO.puts "10日後の日付は #{future_date} です。"
# Output: 10日後の日付は 2021-10-27 です。

# 昨日から1ヶ月前の日付を計算する
yesterday = Date.today()
past_date = Date.subtract(yesterday, 30, :day)
IO.puts "1ヶ月前の日付は #{past_date} です。"
# Output: 1ヶ月前の日付は 2021-09-29 です。
```

## ディープダイブ

日付を未来や過去に計算する際に注意すべき点があります。まず、 `Date.add/2` と `Date.subtract/2` は第二引数で指定した単位に日付を加えたり引いたりするため、単位を指定することが重要です。また、日付を操作する際にはタイムゾーンにも気を配る必要があります。

さらに、Elixirでは日付を扱うために `Date` モジュールの他に、 `DateTime` モジュールや `NaiveDateTime` モジュールなども利用することができます。それぞれのモジュールには異なるメソッドがあり、日付操作のニーズに合わせて使い分けることができます。

## 参考リンク

- [Elixir公式ドキュメント - Elixir.DateTime](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir公式ドキュメント - Elixir.Date](https://hexdocs.pm/elixir/Date.html)
- [Elixir公式ドキュメント - Elixir.NaiveDateTime](https://hexdocs.pm/elixir/NaiveDateTime.html)

## 関連リンク

- [Elixirプログラミング入門記事一覧](https://www.example.com/elixir-programming)