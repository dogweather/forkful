---
title:                "未来や過去の日付を計算する"
html_title:           "C#: 未来や過去の日付を計算する"
simple_title:         "未来や過去の日付を計算する"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ？
計算が必要になる理由はいくつかありますが、最も一般的な理由は、将来のイベントや予定を計画するために必要な日付を知る必要があるためです。また、過去の日付を調べることで、歴史的なデータを分析したり、調査したりすることができます。

## 使い方
まずはDateTimeクラスを使用して今日の日付を取得します。例えば、以下のように記述します。

```C#
DateTime today = DateTime.Today;
```

それから、AddDaysメソッドを使用して日付を変更することができます。例えば、10日後の日付を取得したい場合、以下のように記述します。

```C#
DateTime futureDate = today.AddDays(10);
```

また、日付を変更するだけでなく、特定の日付の曜日や月を取得することもできます。例えば、日付が金曜日かどうかを確認するには、以下のように記述します。

```C#
if(futureDate.DayOfWeek == DayOfWeek.Friday)
{
    Console.WriteLine("It's a Friday!");
}
```

## ディープダイブ
日付を計算する際には、注意すべき点がいくつかあります。例えば、うるう年の処理や、タイムゾーンの考慮などです。また、日付のフォーマットを変更する方法なども知る必要があります。詳細は公式のドキュメントや他のオンラインリソースを参考にしてください。

## 参考リンク
- [DateTime クラスのドキュメント](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [C# での日付と時刻の操作方法](https://docs.microsoft.com/en-us/dotnet/standard/datetime/working-with-dates-and-times)