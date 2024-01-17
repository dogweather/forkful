---
title:                "「CSVの操作」"
html_title:           "Bash: 「CSVの操作」"
simple_title:         "「CSVの操作」"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/bash/working-with-csv.md"
---

{{< edit_this_page >}}

## CSVって何？
CSVはComma-Separated Valuesの略で、コンマで区切られたテキストファイルのことを指します。プログラマーがCSVを扱うのは、データを簡単に読み書きするためです。

## 使い方：
Bashを使ってCSVファイルを操作する方法を説明します。まず、CSVファイルを読み込んでデータを表示するには、以下のようなコマンドを使用します。
```Bash
cat file.csv
```
出力例：
```
id, name, age
1, John, 25
2, Sarah, 30
```
CSVファイルはテキストファイルなので、grepやawkなどのテキスト処理コマンドを使用して、データをフィルタリングや加工することもできます。

```Bash
awk -F"," '$3 >= 25 {print $2}' file.csv
```
出力例：
```
John
Sarah
```

## ディープダイブ：
CSVは1970年代にIBMで開発された非常にシンプルで普遍的なデータ形式です。今日ではさまざまなデータベースやスプレッドシートでサポートされています。また、PythonやJavaなどのプログラミング言語でもCSVを扱うことができます。

## 関連リンク：
- [IBMのCSVの歴史](https://www.ibm.com/docs/en/bpm/8.5.7?topic=overview-csv-file-format)
- [Bashのマニュアルページ](https://www.gnu.org/software/bash/manual/bash.html)
- [awkのチュートリアル](https://www.thegeekstuff.com/2010/02/awk-conditional-statements/)