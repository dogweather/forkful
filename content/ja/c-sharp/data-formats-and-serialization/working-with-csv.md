---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:28.128987-07:00
description: "\u2026"
lastmod: '2024-03-11T00:14:15.729660-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "CSV\u3068\u306E\u4F5C\u696D"
---

{{< edit_this_page >}}

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
