---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:28.128987-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.153821-06:00'
model: gpt-4-0125-preview
summary: "CSV\uFF08\u30AB\u30F3\u30DE\u533A\u5207\u308A\u5024\uFF09\u30D5\u30A1\u30A4\
  \u30EB\u306F\u3001\u30D7\u30EC\u30FC\u30F3\u30C6\u30AD\u30B9\u30C8\u3067\u8868\u5F62\
  \u5F0F\u306E\u30C7\u30FC\u30BF\u3092\u8868\u3057\u3001\u500B\u3005\u306E\u5024\u3092\
  \u533A\u5207\u308B\u305F\u3081\u306B\u30AB\u30F3\u30DE\u3092\u4F7F\u7528\u3059\u308B\
  \u4E00\u822C\u7684\u306A\u30C7\u30FC\u30BF\u4EA4\u63DB\u5F62\u5F0F\u3067\u3059\u3002\
  \u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001CSV\u30D5\u30A1\u30A4\u30EB\u3092\
  \u4F7F\u3063\u3066\u3001\u3055\u307E\u3056\u307E\u306A\u30A2\u30D7\u30EA\u30B1\u30FC\
  \u30B7\u30E7\u30F3\u3084\u30B5\u30FC\u30D3\u30B9\u9593\u3067\u30C7\u30FC\u30BF\u306E\
  \u30A4\u30F3\u30DD\u30FC\u30C8\u3001\u30A8\u30AF\u30B9\u30DD\u30FC\u30C8\u3001\u304A\
  \u3088\u3073\u64CD\u4F5C\u3092\u5BB9\u6613\u306B\u884C\u3046\u305F\u3081\u306B\u53D6\
  \u308A\u7D44\u3093\u3067\u3044\u307E\u3059\u3002\u3053\u308C\u306F\u3001\u30B9\u30D7\
  \u30EC\u30C3\u30C9\u30B7\u30FC\u30C8\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\
  \u3001\u30C7\u30FC\u30BF\u30D9\u30FC\u30B9\u3001\u304A\u3088\u3073\u30D7\u30ED\u30B0\
  \u30E9\u30DF\u30F3\u30B0\u8A00\u8A9E\u3068\u4E92\u63DB\u6027\u306E\u3042\u308B\u30B7\
  \u30F3\u30D7\u30EB\u3067\u5E83\u304F\u30B5\u30DD\u30FC\u30C8\u3055\u308C\u3066\u3044\
  \u308B\u5F62\u5F0F\u3060\u304B\u3089\u3067\u3059\u3002."
title: "CSV\u3068\u306E\u4F5C\u696D"
weight: 37
---

## 何となぜ？
CSV（カンマ区切り値）ファイルは、プレーンテキストで表形式のデータを表し、個々の値を区切るためにカンマを使用する一般的なデータ交換形式です。プログラマーは、CSVファイルを使って、さまざまなアプリケーションやサービス間でデータのインポート、エクスポート、および操作を容易に行うために取り組んでいます。これは、スプレッドシートアプリケーション、データベース、およびプログラミング言語と互換性のあるシンプルで広くサポートされている形式だからです。

## 方法：
C#でCSVファイルを扱うには、基本的な操作には`System.IO`名前空間を使用し、より複雑な操作を行ったり、大きなファイルをスムーズに扱うためには、`CsvHelper`のようなサードパーティのライブラリを検討することができます。以下に、両方のアプローチを使用してCSVファイルから読み込み、CSVファイルへ書き込む方法の例を示します。

### System.IOを使用してCSVファイルを読み込む
```csharp
using System;
using System.IO;

class ReadCSV
{
    static void Main()
    {
        string filePath = @"path\to\your\file.csv";
        // CSVファイルのすべての行を読み込む
        string[] csvLines = File.ReadAllLines(filePath);
        
        foreach (string line in csvLines)
        {
            string[] rowData = line.Split(',');
            Console.WriteLine($"一列目: {rowData[0]}, 二列目: {rowData[1]}");
        }
    }
}
```

**サンプル出力：**
```
一列目: Name, 二列目: Age
一列目: John Doe, 二列目: 30
```

### System.IOを使用してCSVファイルに書き込む
```csharp
using System;
using System.Collections.Generic;
using System.IO;

class WriteCSV
{
    static void Main()
    {
        string filePath = @"path\to\your\output.csv";
        var lines = new List<string>
        {
            "Name,Age",
            "John Doe,30",
            "Jane Smith,25"
        };
        
        File.WriteAllLines(filePath, lines);
        Console.WriteLine("CSVファイルが書き込まれました。");
    }
}
```

**サンプル出力：**
```
CSVファイルが書き込まれました。
```

### CsvHelperを使用してCSVを読む
CsvHelperを使用するには、まずNuGetパッケージマネージャーを使ってプロジェクトに`CsvHelper`パッケージを追加します。

```csharp
using CsvHelper;
using System.Globalization;
using System.IO;
using System.Linq;
using CsvHelper.Configuration;

class ReadCSVWithCsvHelper
{
    static void Main()
    {
        string filePath = @"path\to\your\file.csv";

        using (var reader = new StreamReader(filePath))
        using (var csv = new CsvReader(reader, CultureInfo.InvariantCulture))
        {
            var records = csv.GetRecords<dynamic>().ToList();
            foreach (var record in records)
            {
                Console.WriteLine($"一列目: {record.Name}, 二列目: {record.Age}");
            }
        }
    }
}
```

**サンプル出力：**
```
一列目: John Doe, 二列目: 30
一列目: Jane Smith, 二列目: 25
```

### CsvHelperを使用してCSVに書き込む
```csharp
using CsvHelper;
using System.Globalization;
using System.IO;
using System.Collections.Generic;
using CsvHelper.Configuration;

class WriteCSVWithCsvHelper
{
    public class Person
    {
        public string Name { get; set; }
        public int Age { get; set; }
    }

    static void Main()
    {
        string filePath = @"path\to\your\output.csv";
        var records = new List<Person>
        {
            new Person { Name = "John Doe", Age = 30 },
            new Person { Name = "Jane Smith", Age = 25 }
        };

        using (var writer = new StreamWriter(filePath))
        using (var csv = new CsvWriter(writer, CultureInfo.InvariantCulture))
        {
            csv.WriteRecords(records);
        }
        
        Console.WriteLine("CsvHelperを使ってCSVファイルが書き込まれました。");
    }
}
```

**サンプル出力：**
```
CsvHelperを使ってCSVファイルが書き込まれました。
```
