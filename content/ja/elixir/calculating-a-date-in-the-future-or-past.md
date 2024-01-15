---
title:                "「過去や未来の日付を計算する」"
html_title:           "Elixir: 「過去や未来の日付を計算する」"
simple_title:         "「過去や未来の日付を計算する」"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# なぜ

日々の生活やビジネスで、将来の日付を計算する必要があるシーンは多々あります。特に、プログラムを書いている開発者にとっては、正確な日付計算は重要なスキルです。そこで、Elixirを使って日付の計算を行う方法について説明します。

## 方法

まずは、Elixirの計算機能を使用して日付の計算を行う方法を見ていきましょう。以下のコード例を参考にしてください。

```Elixir
# 現在の日付を取得
current_date = Date.utc_today()

# 1年後の日付を計算
one_year_later = Date.add(current_date, 1, :years)

# 1ヶ月前の日付を計算
one_month_ago = Date.add(current_date, -1, :months)

# 今日を基準とした2週間後の日付を計算
two_weeks_later = Date.add(current_date, 2, :weeks)

# 今日を基準とした1日前の日付を計算
yesterday = Date.add(current_date, -1, :days)
```

上記のコードでは、`Date.utc_today()`を使用して現在の日付を取得し、その日付を基準に`Date.add/3`関数を使用して日付の計算を行っています。第1引数に日付の基準となる日付、第2引数に計算したい数値、第3引数に計算の単位を指定します。詳細はElixirの公式ドキュメントを参考にしてください。

計算結果は、それぞれの日付を表す`Date`モジュールのオブジェクトとして返されます。結果を表示する際は、`inspect()`関数を使用してください。

```Elixir
# 計算結果の表示例
IO.puts("1年後の日付は #{inspect(one_year_later)}です。")
```

## 深堀り

Elixirでは、日付の計算だけでなく、様々な日時操作を行うことができます。例えば、`DateTime`モジュールを使用することで、時刻の計算やフォーマットも簡単に行うことができます。

また、Elixirには日付の扱いに特化したライブラリである`Timex`が存在します。`Timex`を使用することで、より複雑な日付の計算や、ロケールに応じた日付表示なども行うことができます。

さらに詳細な情報は、Elixirの公式ドキュメントや`Timex`のドキュメントを参考にしてください。

## 参考リンク

- Elixir 公式ドキュメント: https://hexdocs.pm/elixir/Date.html
- `Timex` ドキュメント: https://hexdocs.pm/timex/readme.html