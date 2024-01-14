---
title:                "Elixir: 「CSVファイルの操作」"
simple_title:         "「CSVファイルの操作」"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## なぜCSVを使用するのか

CSV（Comma-Separated Values）はデータやテーブルを保存しやすくするためによく使用されるフォーマットです。Elixirでは、CSVを扱うための様々なライブラリがあります。

## 使い方

CSVを扱う方法はいくつかありますが、ここではElixirの`CSV`モジュールを使用する方法を紹介します。まずは下記のように`CSV`モジュールを呼び出しましょう。

```Elixir
require CSV
```

次に、CSVファイルからデータを読み込むには`CSV.read`関数を使用します。例えば、下記のようなCSVファイルがあるとします。

```
id,name,age
1,John,25
2,Alice,30
3,Bob,40
```

このファイルを読み込むには次のようにします。

```Elixir
data = CSV.read("example.csv")
```

`data`には以下のようなリストが返ってきます。

```
[
  ["id", "name", "age"],
  ["1", "John", "25"],
  ["2", "Alice", "30"],
  ["3", "Bob", "40"]
]
```

また、CSVファイルにデータを書き込むには`CSV.write`関数を使用します。例えば、新しいデータを追加した後に`example.csv`ファイルを上書きする場合は次のようにします。

```Elixir
new_data = [["4", "Sarah", "35"]]
CSV.write("example.csv", new_data)
```

## CSVを深く掘り下げる

`CSV`モジュールには`read`と`write`以外にも様々な関数が用意されています。例えば、ファイルのエンコーディングを指定したり、データを加工するための関数も使用することができます。

また、Elixirのプロジェクトでは`CSV`モジュールを利用するだけでなく、[Floki](https://github.com/philss/floki)や[TinyCSV](https://github.com/djm/tiny_csv)などのライブラリを組み合わせて使うことで、より柔軟にCSVを操作することができます。

## See Also

- [CSVモジュールのドキュメント](https://hexdocs.pm/elixir/CSV.html)
- [Flokiライブラリ](https://github.com/philss/floki)
- [TinyCSVライブラリ](https://github.com/djm/tiny_csv)