---
title:                "Fish Shell: 「csv を扱う」"
simple_title:         "「csv を扱う」"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## なぜ

CSVファイルを扱うことに興味があるのであれば、Fish Shellプログラミングはとても便利で効率的な方法です。

## どのように

CSVファイルを扱う最も基本的な方法は、Fish Shellの内部コマンドである`csvformat`を使用することです。これは、CSVファイルをテーブル形式にフォーマットすることができます。

例えば、以下のようにコマンドを入力すると、CSVファイルの内容がテーブル形式で表示されます。

```Fish Shell
csvformat my_file.csv
```

出力は以下のようになります。

```Fish Shell
Column 1    Column 2    Column 3
Value 1     Value 2     Value 3
Value 4     Value 5     Value 6
```

また、CSVファイルをフィルタリングすることもできます。例えば、以下のようにコマンドを入力すると、2列目が"red"の行のみを表示します。

```Fish Shell
csvformat my_file.csv | awk -F '\"*,\"*' '$2 == "red"'
```

出力は以下のようになります。

```Fish Shell
Column 1    Column 2    Column 3
Value 1     red         Value 3
```

## 深堀り

CSVファイルを扱う際に知っておくべき重要なことの一つは、文字符号化方式です。ファイルによって異なるため、`--lineterminator`オプションを使用して、正しい行終端記号を指定する必要があります。

また、Fish Shellでは、`csvformat`の代わりに`csvcut`コマンドを使用することもできます。これは、特定の列を切り出したり、特定の文字列に基づいてフィルタリングしたりすることができます。

さらに、Fish Shellでは、他のシェルよりも柔軟性が高いため、CSVファイルの扱い方は無限にあります。様々な拡張機能を活用することで、自分にとって最適な形でCSVファイルを扱うことができます。

## 関連リンク

- [Fish Shell公式サイト](https://fishshell.com/)
- [Fish Shellドキュメンテーション](https://fishshell.com/docs/current/index.html)
- [Fish Shell Cookbook: CSV manipulation](https://isene.org/2017/11/Fish-Cookbook-CSV.html)
- [CSVファイルをフィルタリングする方法](https://superuser.com/questions/903168/how-to-select-and-delete-columns-with-awk-in-csv-file)