---
title:                "C#: 「csv との作業」"
simple_title:         "「csv との作業」"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## なぜCSVを扱うか

CSVファイルは、データの保存や転送において広く使用され、非常に便利です。C#を使ってCSVファイルを扱うことで、データベースやエクセルファイルなどとのデータの受け渡しや分析が簡単になります。

## 手順

CSVを扱うには、CSVファイルからデータを読み込んだり、データをCSVファイルに書き込む必要があります。それぞれの手順を以下のコード例とサンプル出力で説明します。

```C#
// CSVファイルの読み込み
string[] lines = File.ReadAllLines("data.csv"); // ファイルパス
foreach (string line in lines)
{
    // データをカンマで分割して配列に格納
    string[] data = line.Split(",");
    // 表示
    Console.WriteLine("名前: " + data[0]);
    Console.WriteLine("年齢: " + data[1]);
    Console.WriteLine("職業: " + data[2]);
    Console.WriteLine();
}

// CSVファイルへの書き込み
// データを配列に格納
string[] data1 = { "山田太郎", "30", "会社員" };
string[] data2 = { "鈴木花子", "25", "学生" };
// ファイルを開いてデータを書き込む
using (var writer = new StreamWriter("data.csv")) // ファイルパス
{
    // 一行ずつデータを書き込む
    writer.WriteLine(string.Join(",", data1));
    writer.WriteLine(string.Join(",", data2));
}

// サンプル出力
// 名前: 山田太郎
// 年齢: 30
// 職業: 会社員
//
// 名前: 鈴木花子
// 年齢: 25
// 職業: 学生

```

## CSVの詳細

CSVファイルは、カンマでデータを区切ることでデータを管理します。しかし、製品名などの文字列にカンマが含まれている場合や、データの量が多い場合には、別の文字で区切ることが推奨されます。また、書き込まれたデータの順番や型が異なるファイルを読み込むとエラーが起きることがあるため、データの整合性を確認する必要があります。

See Also（参考リンク）

- [C# で CSV ファイルを扱う方法](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/file-system/how-to-read-from-a-csv-file)
- [CSVファイルを扱う際の注意点](https://www.codeproject.com/Articles/9258/A-Portable-and-Efficient-Generic-Parser-for-Flat-F)