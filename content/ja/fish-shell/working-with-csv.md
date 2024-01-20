---
title:                "「CSV を扱う」"
html_title:           "Fish Shell: 「CSV を扱う」"
simple_title:         "「CSV を扱う」"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/fish-shell/working-with-csv.md"
---

{{< edit_this_page >}}

## なに & なぜ？
CSVとは何か、それをプログラマーがする理由を約2〜3文で説明します。

CSVとは、コンマで区切られたテキストファイルの形式のことで、データを表形式で保存するためによく使用されます。プログラマーがCSVを扱う理由は、データを取り出したり、処理したりするために簡単な方法を提供するからです。

## 使い方：
下のコードブロック内のコーディング例とサンプル出力を使い、Fish ShellでCSVを処理する方法をご紹介します。

```
Fish Shellを試すことで、CSVを簡単に操作できることがわかるでしょう。まず、CSVファイルを作成し、データを入力します。

$ vim sample.csv

次に、CSVファイルを読み込んで、データを表示します。

$ fish -c "while read line; echo $line; end < sample.csv"

このコマンドを実行すると、sample.csvファイルに入力されたデータが表示されます。  

```

## 詳細を探求:
ここではCSVについての歴史的な文脈、代替手段、そしてCSV処理の実装詳細について説明します。

### 歴史的文脈：
CSVは1972年に最初に開発され、当初はメインフレームコンピューターのデータベースで使用されていました。その後、パーソナルコンピュータが普及するにつれ、CSVはより一般的に使用されるようになりました。

### 代替手段：
CSV以外にも、データを表形式で保存するためのさまざまなフォーマットがあります。例えば、XMLやJSON、SQLなどがあります。

### 実装詳細：
Fish Shellの内部コードは、Open CSV - Java CSVライブラリを使用して書かれています。また、`fish -c`コマンドを使用することで、1行ずつCSVを読み込み、データを処理することができます。

## 関連情報:
CSVについてさらに学ぶためのリンクをいくつか紹介します。

- [Open CSV - Java CSV library](http://opencsv.sourceforge.net/)
- [CSVとは - Wikipedia](https://ja.wikipedia.org/wiki/Comma-Separated_Values)