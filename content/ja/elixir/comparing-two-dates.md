---
title:                "Elixir: 「二つの日付を比較する」"
simple_title:         "「二つの日付を比較する」"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

日付を比較することの重要性は、よく知られています。日付を比較することによって、特定の日付が過去、現在、または未来のどの時点に属するかを判断することができます。これは、データを分析したり、アプリケーションのロジックを決定したりする際に非常に役立ちます。Elixirプログラミングの世界では、日付の比較は非常に重要であり、その方法を知ることは非常に役に立ちます。

## 方法

Elixirでは、DateTimeモジュールを使用して日付の比較を行うことができます。以下のコード例を参考にしてください。

```Elixir
# 2つの日付を定義する
date1 = ~D[2020-01-01]
date2 = ~D[2021-01-01]

# 比較演算子を使用して日付を比較する
date1 < date2
#=> true

date1 > date2
#=> false

# 日付に関する様々な情報を取得することができる
DateTime.diff(date1, date2)
#=> {:absolute, 31536000}

DateTime.before?(date1, date2)
#=> true

DateTime.same?(date1, date2)
#=> false
```

また、日付の文字列を使用して日付の比較を行うこともできます。

```Elixir
# 日付の文字列を定義する
date1 = "2020-01-01"
date2 = "2021-01-01"

# 文字列をDateTimeオブジェクトに変換する
DateTime.from_iso8601(date1)
#=> {:ok, ~D[2020-01-01]}

DateTime.from_iso8601(date2)
#=> {:ok, ~D[2021-01-01]}

# DateTimeオブジェクトを使用して日付を比較する
DateTime.from_iso8601(date1) < DateTime.from_iso8601(date2)
#=> true
```

## ディープダイブ 

日付の比較を行う際に、注意すべき点があります。Elixirでは、日付を比較する際にタイムゾーンを考慮することができます。デフォルトでは、UTCタイムゾーンが使用されますが、必要に応じてタイムゾーンを指定することもできます。

また、DateTimeモジュールには、日付を操作するためのさまざまな便利な関数が用意されています。このモジュールを上手に活用することで、日付の比較をより効率的に行うことができます。

## 参考リンク

- [DateTime — Elixir v1.12.2](https://hexdocs.pm/elixir/DateTime.html)
- [日付と時刻 — Elixir v1.12.2](https://elixirschool.com/ja/lessons/basics/basics-and-datetime/) 

## 参考文献

- [Elixir v1.12.2 日付と時刻](https://hexdocs.pm/elixir/DateTime.html)