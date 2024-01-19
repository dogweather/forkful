---
title:                "日付を文字列に変換する"
html_title:           "C++: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？ (What & Why?)
日付を文字列に変換するとは、日時データを指定した形式の文字列に変換することです。これは、データの表示や日時データのシリアライズ及びデシリアライズの際に行います。

## どうやって？ (How to?)
```C#
using System;

public class Program
{
    public static void Main()
    {
        DateTime date = DateTime.Now;
        string dateToString = date.ToString("MM/dd/yyyy");
        Console.WriteLine(dateToString);
    }
}
```
これで出力はこんな感じになります:
```C#
"12/01/2022"
```

## ディープダイブ (Deep Dive)
### 歴史的文脈
.NET フレームワークが策定された当初から、日付を自由な形式の文字列に変換する機能が提供されています。

### 代替手段
Date.ToStringメソッドの他にも、フォーマット指定子を含むString.Formatメソッドや、より自由度の高い自作メソッドなどを利用することも可能です。

### 実装詳細
ToStringメソッドは、日付を文字列に変換する際、適用される特定の書式を指定することが可能です。

## 参照資料 (See Also)
- [String Formatting](https://docs.microsoft.com/ja-jp/dotnet/api/system.string.format?view=net-6.0)
- [Standard Date and Time Format Strings](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/standard-date-and-time-format-strings)