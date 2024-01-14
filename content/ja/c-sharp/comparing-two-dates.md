---
title:                "C#: 「2つの日付の比較」"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

日付を比較する理由は、プログラムで日付を扱う必要があるからです。たとえば、過去のイベントの日付をチェックするために、今日の日付と比較する必要があります。

## 方法

C#で2つの日付を比較するには、DateTimeクラスのCompareメソッドを使用します。以下のようにコードを書くことができます。

```C#
DateTime date1 = new DateTime(2020, 4, 1);
DateTime date2 = new DateTime(2020, 4, 15);

Console.WriteLine(DateTime.Compare(date1, date2));
```

このコードでは、date1がdate2よりも前の日付であるため、-1が出力されます。

## ディープダイブ

DateTime.Compareメソッドでは、2つの日付を比較する際により詳細なオプションを指定することができます。例えば、日付のみで比較するのではなく、時刻やタイムゾーンも考慮することができます。また、比較する日付の精度をミリ秒レベルまで設定することもできます。

## その他の関連記事

[DateTime.Compare メソッドのドキュメント](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime.compare?view=netcore-3.1)

[日付と時刻の比較に関するC#のヒント](https://qiita.com/watarun54/items/cdc931d26184fd03df5f)