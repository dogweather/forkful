---
title:                "C#: 将来または過去の日付の計算"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ日付を計算するのか

日常生活やビジネスにおいて、将来の日付や過去の日付を計算する必要があることがあります。例えば、誕生日や結婚記念日の追跡や、契約書の期限確認などです。C#を用いて日付を計算することで、正確で効率的な処理が可能になります。

## 日付の計算方法

C#ではDateTimeクラスを使用して日付の計算が行えます。具体的なコーディング例を示します。

```
using System;

namespace DateCalculation
{
    class Program
    {
        static void Main(string[] args)
        {
            // 今日の日付を取得
            DateTime today = DateTime.Today;

            // 10日後の日付を計算
            DateTime futureDate = today.AddDays(10);

            // 10日前の日付を計算
            DateTime pastDate = today.AddDays(-10);

            // 結果を出力
            Console.WriteLine("今日の日付は: " + today.ToString("MM/dd/yyyy"));
            Console.WriteLine("10日後の日付は: " + futureDate.ToString("MM/dd/yyyy"));
            Console.WriteLine("10日前の日付は: " + pastDate.ToString("MM/dd/yyyy"));

            Console.ReadLine();
        }
    }
}

```

上記のコードを実行すると、今日の日付から10日後と10日前の日付が計算され、以下のように出力されます。

```
今日の日付は: 11/17/2021
10日後の日付は: 11/27/2021
10日前の日付は: 11/07/2021
```

## 日付計算の詳細解説

日付を計算する際には、DateTimeクラスのAddDaysメソッドを使用します。このメソッドは、引数に指定した日数を加算または減算して計算結果を返します。

さらに、DateTimeクラスにはAddMonthsやAddYearsといったメソッドもあり、日付の月や年を加算または減算することも可能です。加算や減算する前後の日付の範囲や、閏年における計算結果なども考慮されているため、日付計算の精度が高くなります。

## 関連リンク

- [C# DateTimeクラスのドキュメント](https://docs.microsoft.com/en-us/dotnet/api/system.datetime)
- [C#ニューララーニング日本語ドキュメント](https://docs.microsoft.com/ja-jp/dotnet/csharp/)

See Also:
リンクのリストをここに記載