---
title:                "「二つの日付の比較」"
html_title:           "C#: 「二つの日付の比較」"
simple_title:         "「二つの日付の比較」"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

誰でも、プログラミングの問題を解決する際には、日付を比較する必要があります。しかし、C#には複数の日付間の比較を行うための様々なオプションが存在するため、どの方法が最も適しているかを知ることは重要です。この記事では、二つの日付を比較する方法について説明します。

## 方法

まず、初めに```DateTime.Compare```メソッドを使用する方法を紹介します。これは、二つの日付を比較し、その結果を整数値で返します。返される値によって、どちらの日付の方が大きいか、同じかを判別することができます。

```C#
DateTime date1 = new DateTime(2020, 10, 1);
DateTime date2 = new DateTime(2020, 11, 1);

int result = DateTime.Compare(date1, date2);

if (result < 0)
{
    Console.WriteLine("date1はdate2よりも前の日付です。");
}
else if (result == 0)
{
    Console.WriteLine("date1とdate2は同じ日付です。");
}
else if (result > 0)
{
    Console.WriteLine("date1はdate2よりも後の日付です。");
}

// Output: date1はdate2よりも前の日付です。
```

次に、```DateTime.Equals```メソッドを使用する方法を紹介します。このメソッドを使用すると、二つの日付が完全に同じ日付かどうかを判別することができます。

```C#
DateTime date1 = new DateTime(2020, 10, 1);
DateTime date2 = new DateTime(2020, 11, 1);

if (DateTime.Equals(date1, date2))
{
    Console.WriteLine("date1とdate2は同じ日付です。");
}
else
{
    Console.WriteLine("date1とdate2は同じ日付ではありません。");
}

// Output: date1とdate2は同じ日付ではありません。
```

最後に、```DateTime.DayOfYear```プロパティを使用する方法を紹介します。このプロパティは、二つの日付の日数を比較し、その結果を整数値で返します。返される値によって、どちらの日付が過去か、同じ日数かを判別することができます。

```C#
DateTime date1 = new DateTime(2020, 10, 1);
DateTime date2 = new DateTime(2020, 11, 1);

if (date1.DayOfYear < date2.DayOfYear)
{
    Console.WriteLine("date1はdate2よりも前の日付です。");
}
else if (date1.DayOfYear == date2.DayOfYear)
{
    Console.WriteLine("date1とdate2は同じ日数です。");
}
else if (date1.DayOfYear > date2.DayOfYear)
{
    Console.WriteLine("date1はdate2よりも後の日付です。");
}

// Output: date1はdate2よりも前の日付です。
```

## ディープダイブ

以上の方法は、二つの日付を比較するための基本的な方法です。しかし、C#にはさらに複雑な比較方法が存在します。例えば、特定の時間帯の比較や、日付が同じだが時差がある場合の比較などがあります。詳細については、Microsoftのドキュメント[「日付の比較」](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/working-with-dates)を参照してください。

## 同様に見てみる

- [「C# における時間の処理」](https://www.ud