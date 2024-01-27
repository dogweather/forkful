---
title:                "CSVファイルの操作"
date:                  2024-01-19
html_title:           "Arduino: CSVファイルの操作"
simple_title:         "CSVファイルの操作"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
「なに？」「なぜ？」
CSV（Comma-Separated Values）は、データを表形式で保存・共有するシンプルなファイル形式だ。データ交換が簡単で、多くのプログラムに対応しているため、プログラマはよく利用する。

## How to:
「実装方法」
```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string csvPath = "example.csv";
        
        // CSV読み込み
        string[] csvLines = File.ReadAllLines(csvPath);
        foreach (string line in csvLines)
        {
            string[] columns = line.Split(',');
            Console.WriteLine($"名前: {columns[0]}, 年齢: {columns[1]}");
        }
        
        // CSV書き込み
        string newCsvPath = "new_example.csv";
        string[] newCsvLines = { "山田,30", "鈴木,25" };
        File.WriteAllLines(newCsvPath, newCsvLines);
    }
}
```
```
出力:
名前: 山田, 年齢: 30
名前: 鈴木, 年齢: 25
```

## Deep Dive
「詳細な情報」
CSVは1970年代から使われている。JSONやXMLなどの代替フォーマットもあるが、単純さが特徴。`File.ReadAllLines`と`.Split`を使い、行を読み、カンマで分割して扱う。ライブラリもあるが、小規模なデータの場合には必要ないことが多い。

## See Also
「関連情報」
- Microsoft CSV ドキュメント: https://docs.microsoft.com/ja-jp/dotnet/api/system.io.file?view=netcore-3.1
- CsvHelper ライブラリ: https://joshclose.github.io/CsvHelper/ 
- RFC 4180, CSVに関する仕様書: https://tools.ietf.org/html/rfc4180
