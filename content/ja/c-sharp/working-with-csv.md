---
title:                "「CSVの扱い方」"
html_title:           "C#: 「CSVの扱い方」"
simple_title:         "「CSVの扱い方」"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

# 何＆なぜ?

CSVとは、Comma Separated Values (カンマ区切り値) の略称で、データを表形式で保存するためのファイル形式です。プログラマーは、データを処理しやすい形式で保存するために、CSVを使用します。

# 方法：

```C#
// CSVファイルを読み込む
using System;
using System.IO; // System.IOパッケージを使用する

string[] lines = File.ReadAllLines("file.csv"); // CSVファイルを読み込む
foreach(string line in lines){ // 各行に対して処理を行う
    string[] values = line.Split(','); // 各行をカンマで分割し、配列に格納する
    foreach(string value in values){ // 各値に対して処理を行う
        Console.WriteLine(value); // 値を表示する
    }
}

// CSVファイルを書き込む
string[] lines = {"1,2,3", "4,5,6", "7,8,9"}; // CSVファイルに書き込むデータ
File.WriteAllLines("file.csv", lines); // ファイルを書き込む

```

# 深堀り

CSVは、1970年代にIBMによって開発されたデータ形式です。プログラマー以外にも、ビジネスや研究分野などでもよく使用されます。他にも、JSONやXMLといった形式もデータ処理に使用されますが、CSVはよりシンプルな構造を持っているため、データを扱いやすいという利点があります。CSVファイルは、Microsoft ExcelやGoogle Sheetsなどのプログラムでも扱うことができます。

# 関連情報

- [Wikipedia: CSV](https://ja.wikipedia.org/wiki/CSV)