---
title:                "C#: 「二つの日付の比較」"
simple_title:         "「二つの日付の比較」"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

日常生活で、私たちはしばしば日付を比べる必要があります。例えば、イベントの日時を決める際や、過去のデータと現在のデータを比較する際などです。そんなとき、プログラマーとしては、日付を比べる方法を知っていることが重要です。今回は、C#で日付を比べる方法についてご紹介します。

## 方法

まずは、日付を比べる前に、DateTimeというデータ型について知っておきましょう。DateTimeは、日付と時刻を扱うための便利なデータ型です。

例えば、今日の日付を取得するには、`DateTime.Now.Date`という表記を使います。同じように、昨日の日付を取得するには、`DateTime.Now.Date.AddDays(-1)`という表記を使います。

次に、日付を比べる方法について見ていきましょう。以下のようなコードを使うことで、二つの日付の大小関係を比較することができます。

```C#
DateTime date1 = new DateTime(2020, 10, 10);
DateTime date2 = new DateTime(2020, 10, 15);

if (date1 < date2)
{
    Console.WriteLine("date1はdate2よりも前の日付です。");
}
else if (date1 > date2)
{
    Console.WriteLine("date1はdate2よりも後の日付です。");
}
else
{
    Console.WriteLine("date1とdate2は同じ日付です。");
}

// Output: date1はdate2よりも前の日付です。
```

このように、比較したい二つの日付をDateTime型の変数として宣言し、if文を使って比較することで、大小関係を判定することができます。

また、時刻を含めて比較する場合は、`date1 < date2`の代わりに、`date1.Ticks < date2.Ticks`という表記を使うことで、より詳細な比較が可能になります。

## 詳しく見る

DateTime型には、様々な便利なメソッドが用意されています。例えば、二つの日付の間に何日間があるかを計算するには、`date1.Subtract(date2).Days`というコードを使うことができます。他にも、曜日や月の情報を取得することも可能です。

また、時差や夏時間の考慮など、より詳細な日付の比較を行う場合は、NodaTimeという外部ライブラリを使うことができます。NodaTimeを使うことで、さまざまなカレンダーシステムに対応した日付の比較を行うことができます。

## それではまた！

今回は、C#で日付を比べる方法について見てきました。日々のプログラミングの中で、日付を比べることがあるかもしれません。ぜひ、この記事を参考にして、効率的なプログラミングをしていきましょう！

## 参考リンク

- [DateTime構造体 (System) - Microsoft Docs](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime?view=net-5.0)
- [NodaTime/NodaTime - GitHub](https://github.com/nodatime/nodatime)
- [C# で日付を比較する方法 - 今日からはじ