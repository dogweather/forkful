---
title:                "「将来や過去の日付の計算方法」"
html_title:           "Elixir: 「将来や過去の日付の計算方法」"
simple_title:         "「将来や過去の日付の計算方法」"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 計算された日付とは？
計算された日付とは、現在の日付から将来や過去の日数を加えたり引いたりして算出された日付のことを指します。プログラマーが日付を計算する理由は、例えば誕生日や期限の計算など、日付のさまざまな操作が必要になるためです。

## 方法：
以下のElixirコードブロックを参考に、将来や過去の日付を計算する方法をご紹介します。

```Elixir
# 現在の日付を取得する
now = Date.utc_today()

# 各日付の単位で操作を行う
# 例えば、次の週の同じ曜日を計算する場合
next_week = Date.add(now, 7, :days)
```

## 深堀り：
将来や過去の日付を計算することは、一般的にはプログラミング言語でサポートされています。しかし、その方法は言語によって異なります。Elixirでは、Dateモジュールを使用して日付の操作を行います。また、使用できる単位も日付や月など多様です。

## 関連情報：
以下の参考資料をご覧ください。
- [Elixir 公式ドキュメント](https://hexdocs.pm/elixir/master/Date.html)
- [Elixir Dateモジュールの使用例](https://elixir-lang.org/getting-started/working-with-dates-and-times.html#date-manipulation)