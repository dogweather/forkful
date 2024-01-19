---
title:                "2つの日付を比較する"
html_title:           "Elixir: 2つの日付を比較する"
simple_title:         "2つの日付を比較する"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何となぜ?

日付の比較とは、一つの日付が他の日付よりも前、後、または同じであるかを判定することです。この操作はフィルタリング、ソート、バリデーション等のロジックによく使用されます。

## 使い方：

```Elixir
defmodule DateCompare do
  def compare(date1, date2) do
    Date.compare(date1, date2)
  end
end

date1 = ~D[2022-09-01]
date2 = ~D[2022-10-01]

IO.puts DateCompare.compare(date1, date2)  # outputs: :lt
IO.puts DateCompare.compare(date2, date1)  # outputs: :gt
IO.puts DateCompare.compare(date1, date1)  # outputs: :eq
```

このコードは二つの日付を比較し、:lt (:less_than)、:gt (:greater_than)、または:eq (:equal)を返します。

## ディープダイブ

日付の比較はプログラミングの初期から存在し、アルゴリズムの基礎を形成しています。Elixirの場合、`Date.compare/2`関数により簡単に日付の比較が可能です。

また、他の方法としては、日付をタイムスタンプに変換して比較する方法もあります。その際、`DateTime.to_unix/1`を使うことでタイムスタンプを獲得できます。

ただし、Elixirでは`Date.compare/2`を使うことが推奨されています。この関数は内部的にISO8601日付文字列に変換して比較を行っています。

## 関連資料

以下のリンクを参考にしてください：

- Elixir公式ドキュメント: [Date.compare/2](https://hexdocs.pm/elixir/Date.html#compare/2)
- ElixirのDateTimeモジュール: [DateTime.to_unix/1](https://hexdocs.pm/elixir/DateTime.html#to_unix/1)
- ISO8601について: [Wikipedia - ISO8601](https://ja.wikipedia.org/wiki/ISO_8601)