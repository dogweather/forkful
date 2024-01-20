---
title:                "未来または過去の日付を計算する"
html_title:           "C#: 未来または過去の日付を計算する"
simple_title:         "未来または過去の日付を計算する"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 未来または過去の日付の計算: C# のガイド

## 何となぜ?

日付の計算とは、過去または未来の特定の日付を特定するためのものです。これは、特定のイベントの予定日を記録したり、特定の日数後の日付を知る必要がある場合にプログラマーが行います。

## 方法:

以下は、C#で未来の日付を計算するコードの一例です:

```C#
using System;

public class Program
{
    public static void Main()
    {
        DateTime today = DateTime.Now;
        DateTime futureDate = today.AddDays(10);
        
        Console.WriteLine("今日の日付: " + today.ToString("d"));
        Console.WriteLine("10日後の日付: " + futureDate.ToString("d"));
    }
}
```

これを実行すると次のような出力が表示されます:

```C#
今日の日付: 01/01/2022
10日後の日付: 01/11/2022
```

## 深いダイブ

(1)
長い間、開発者は特定の時間操作を手動で行ってきました。しかし、C#のような現代のプログラミング言語では、組み込みの日付と時刻の演算機能を提供しています。

(2)
代替手段としては、自分で日付時間を操作するための関数を作成することも可能です。しかし、これはコードをより複雑にする可能性があり、ミスを引き起こす可能性があります。

(3)
C#では、日付と時刻はDateTimeとTimeSpanオブジェクトとして表されます。それぞれの専用のメソッド（例：AddDays）を使って日付計算をしたり、２つのDateTimeオブジェクトの差をTimeSpanオブジェクトとして取得し、時間の操作を行ったりします。

## 関連情報

- [DateTime Struct (Microsoft Documentation)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [Working with Dates and Times in C# (Pluralsight Blog Post)](https://www.pluralsight.com/guides/working-with-dates-and-times-in-csharp)