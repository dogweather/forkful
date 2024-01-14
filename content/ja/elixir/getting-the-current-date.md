---
title:                "Elixir: 「現在の日付を取得する」"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

現在の日付を取得する理由は何でしょうか？Elixirのプログラミング言語では、日付や時刻を取得するために便利な機能がたくさんあります。例えば、データベースにタイムスタンプを記録する必要がある場合や、特定の操作を実行するために特定の日時を取得する必要がある場合などに、日付を取得する必要が出てきます。

## 方法

では、実際にElixirで現在の日付を取得する方法を見ていきましょう。以下のコード例を参考にしてください。

```Elixir
Date.utc_today()
#=> ~D[2021-09-27]
```

このように、`Date.utc_today()`という関数を使うことで、今日の日付を取得することができます。デフォルトでは、UTC時間を基準として日付を取得するため、必要に応じてローカルのタイムゾーンに変換する必要があります。また、引数として`TimeZone`を渡すことで、特定のタイムゾーンに変換することも可能です。

```Elixir
Date.utc_today("Asia/Tokyo")
#=> #<DateTime(2021-09-27T00:00:00+09:00 Asia/Tokyo JST)>
```

さらに、日付や時間の特定の部分を取得したい場合には、`Calendar.DateTime`モジュールを使います。

```Elixir
Calendar.DateTime.now("Asia/Tokyo")
#=> #<DateTime(2021-09-27T08:44:25+09:00 Asia/Tokyo JST)>
```

## 深堀り

日付を取得する際、UTC時間を基準としていることについてもう少し詳しく見てみましょう。UTCとは、協定世界時(グリニッジ標準時)のことで、全てのタイムゾーンの基準となる時間です。この基準となる時刻を使うことで、世界中での時間を統一することができます。

Elixirでは、`Date.utc_today()`の他にも、`Date.utc_now()`や`Date.utc_today_leap_seconds()`など、さまざまなUTC関連の関数が用意されています。また、`Calendar.ISO`モジュールを使うことで、ISO 8601形式で日付を取得することもできます。

## 他に見るべきもの

- [Elixirの日付と時刻について](https://hexdocs.pm/elixir/Date.html)
- [ElixirのUTC関連関数について](https://hexdocs.pm/elixir/Date.html#utc-functions)
- [ElixirのCalendar.DateTimeモジュールについて](https://hexdocs.pm/elixir/Calendar.DateTime.html)
- [ISO 8601形式について](https://www.iso.org/iso-8601-date-and-time-format.html)