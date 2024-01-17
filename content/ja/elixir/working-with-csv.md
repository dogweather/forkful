---
title:                "「csv との作業」"
html_title:           "Elixir: 「csv との作業」"
simple_title:         "「csv との作業」"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## 何 & なぜ？

CSVファイルとは、コンマで区切られたデータを格納するためのテキストファイルです。プログラマーたちは、データベースにアクセスせずにデータを処理したい場合や、データを簡単に共有する必要がある場合に、CSVを使用します。

## 方法：

```Elixir
# CSVファイルを読み込む
{:ok, data} = File.read("file.csv")

# CSVをリストに変換する
data
|> String.split("\n") 
|> Enum.map(&String.split(&1, ","))
```

上記のコードでは、まずファイルを読み込み、改行で文を分割し、さらにコンマで列を分割してリストに変換します。これにより、データを簡単に処理することができます。

## 詳細説明：

- CSVファイルは、1972年にデータを簡単に共有するために開発されました。
- また、データベースに代わるリレーショナルデータベースの一種としても使用されています。
- 上記のコードは、Elixirの標準ライブラリであるEnumモジュールを使用しています。

## 関連リンク：

- [ElixirのCSVライブラリドキュメント](https://hexdocs.pm/csv/CSV.html)
- [CSVファイルの歴史についての記事](https://en.wikipedia.org/wiki/Comma-separated_values)