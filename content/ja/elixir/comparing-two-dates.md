---
title:                "日付の比較"
html_title:           "Elixir: 日付の比較"
simple_title:         "日付の比較"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ
日付の比較を行うことの重要性を説明すると、日付を比較することで、データベースやアプリケーションで重要な日付に基づいて処理を行うことができるようになります。例えば、急いで処理を行う必要がある支払いの日付や、期限に追われているタスクの締め切り日などが挙げられます。

## 方法
日付を比較するためには、Elixirのビルトイン関数である`Date.compare/2`を使用します。以下のようにコードを記述することで、2つの日付を比較し、比較結果を表示することができます。

```
Elixir
first_date = ~D[2020-01-01]
second_date = ~D[2020-01-15]

comparison_result = Date.compare(first_date, second_date)
IO.puts "Comparison result: #{comparison_result}"
```

上記のコードを実行すると、比較結果が表示されます。比較結果は3種類あり、`-1`は最初の日付が2つ目の日付よりも前、`0`は2つの日付が同じで、`1`は最初の日付が2つ目の日付よりも後を表します。

## ディープダイブ
Elixirの日付モジュールには、2つの異なる日付を比較する他にも、さまざまな便利な関数が用意されています。`Date.compare/2`以外にも、`Date.same_day?/2`を使用することで、2つの日付が同じ日であるかどうかを簡単に判断することができます。さらに、`Date.before?/2`や`Date.after?/2`を使用することで、それぞれ最初の日付が2つ目の日付よりも前や後になるかどうかを判定することができます。

また、日付の比較に加えて、`DateTime`モジュールを使用することで、日付と時間を同時に比較することもできます。さらに詳しい情報は公式のElixirドキュメントを参照してください。

## 参考リンク
- [Elixir公式ドキュメント](https://hexdocs.pm/elixir/Date.html)
- [Elixir School - Dates, Times and Naive Datetimes](https://elixirschool.com/ja/lessons/basics/dates-times-and-naive-datetimes/)
- [ElixirのDateモジュールを使って年齢を計算する](https://blog.yuhiisk.com/archive/20191028/)
- [演算子のオーバーロードでElixirで日付計算してみる](https://blog.yuhiisk.com/archive/20180422/)
- [Elixirで日付処理を楽に行うためのヘルパーモジュール](https://blog.yuhiisk.com/archive/20170716/)