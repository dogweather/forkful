---
title:                "「csv（カンマ区切り）との作業」"
html_title:           "Elixir: 「csv（カンマ区切り）との作業」"
simple_title:         "「csv（カンマ区切り）との作業」"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## なぜCSVを使うのか
CSVはコンピューターでデータを扱う一般的な形式であり、様々なプログラミング言語でサポートされています。ElixirでCSVを扱うことで、大量のデータを効率的に処理し、データベースやスプレッドシートと簡単に連携することができます。

## 使い方
CSVモジュールを使うことで、Elixirで簡単にCSVファイルを読み書きできます。

```
Elixir 
require CSV

# CSVファイルを読み込む
data = CSV.read("data.csv")

# 列ごとにデータを取得する
data |> Enum.at(0) # ヘッダー行を取得
data |> Enum.map(&(&1[0])) # 1列目のデータを取得

# CSVファイルを書き込む
CSV.write("new_data.csv", data)

```

出力例 :

```
{:ok, [["名前", "年齢", "職業"], ["太郎", "30", "会社員"], ["花子", "25", "学生"], ["次郎", "40", "フリーランス"]]}


```

## ディープダイブ
CSVモジュールにはさまざまなオプションが用意されており、データのパースやヘッダーの指定、空のデータの処理などが可能です。また、Elixirで並列処理を行うことで、大量のデータを高速に処理することができます。

## 参考リンク
- [Elixir公式ドキュメント](https://hexdocs.pm/csv/CSV.html)
- [ElixirでCSVを取り扱う方法](https://qiita.com/mto_um/items/3a726743941d1c3d5945)