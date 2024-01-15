---
title:                "「csvとの作業」"
html_title:           "C#: 「csvとの作業」"
simple_title:         "「csvとの作業」"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## なぜCSVを扱うのか
CSVはデータを格納するために最も一般的に使用されるフォーマットの1つです。CSVファイルを読み書きすることができるようになることは、多くの開発者にとって役に立ちます。

## 使い方
CSVファイルを読み込み、データを処理した後でまた新しいCSVファイルにデータを書き込む方法を学ぶために、以下のコード例を参考にしてください。

```C#
// CSVファイルを読み込む
var lines = File.ReadAllLines("sample.csv");
// データを処理する
foreach(var line in lines)
{
    var data = line.Split(',');
    // CSVから取得したデータを使った処理をする
    Console.WriteLine($"Name: {data[0]}, Age: {data[1]}, Gender: {data[2]}");
}
// 新しいCSVファイルにデータを書き込む
using (var writer = new StreamWriter("new.csv"))
{
    writer.WriteLine("Name,Age,Gender");
    writer.WriteLine("John,30,Male");
    writer.WriteLine("Jane,25,Female");
}
```

## CSVの詳細
CSVファイルは、基本的にテキストファイルであり、データをコンマで区切って表現するものです。しかし、データにコンマや改行などの特殊文字が含まれている場合は、その特殊文字をエスケープする必要があります。また、CSVファイル自体にはデータ型の情報が含まれていないため、データの型変換は開発者の責任となります。

## See Also
- [C#でCSVファイルを読み書きする方法](https://programmingwithmosh.com/net/working-with-csv-files-in-csharp/)
- [Microsoftのドキュメント：CSVファイルを読み込む](https://docs.microsoft.com/ja-jp/dotnet/csharp/programming-guide/file-system/how-to-read-a-text-file-one-line-at-a-time)
- [CSVのフォーマットの詳細](https://www.loc.gov/preservation/digital/formats/fdd/fdd000323.shtml)