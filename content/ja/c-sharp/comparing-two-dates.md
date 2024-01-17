---
title:                "二つの日付の比較"
html_title:           "C#: 二つの日付の比較"
simple_title:         "二つの日付の比較"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?

比較するとは、2つの日付を比べることを意味します。プログラマーは日付を比較することで、2つのイベントやタスクの順序を決めたり、過去の日付から未来の日付までの期間を計算したりすることができます。

## How to:

日付の比較には、```DateTime.Compare```メソッドを使用します。以下の例では、5月1日と5月2日を比較しています。

```C#
DateTime date1 = new DateTime(2020, 5, 1);
DateTime date2 = new DateTime(2020, 5, 2);
int result = DateTime.Compare(date1, date2);
Console.WriteLine(result);
```

このコードの出力は「-1」になります。これは、date1よりもdate2の方が未来の日付であることを示しています。もしdate1がdate2よりも未来の日付だった場合、出力は「1」となります。同じ日付であれば、出力は「0」となります。

## Deep Dive:

日付の比較は広く使用されており、歴史的にも重要な役割を果たしてきました。古代ローマでは、日付を比較することで政治的な順序や重要な出来事を決めるために使用されていました。

今日、日付の比較には様々な方法があります。上記の例では、```DateTime.Compare```メソッドを使用しましたが、```DateTime.Equals```やオーバーロードされた演算子「>」や「<」を使用することもできます。

日付の比較は様々なシーンで使用されており、実装方法も異なるため、ケースバイケースで調べる必要があります。

## See Also:

- [DateTime.Compareメソッド (Microsoft Docs)](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime.compare?view=netcore-3.1)
- [DateTime.Equalsメソッド (Microsoft Docs)](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime.equals?view=netcore-3.1)
- [オーバーロードされた演算子 (Microsoft Docs)](https://docs.microsoft.com/ja-jp/dotnet/csharp/language-reference/operators/operator-overloading)