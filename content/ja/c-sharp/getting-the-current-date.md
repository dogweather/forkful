---
title:                "現在の日付を取得する"
date:                  2024-01-20T15:13:46.767241-07:00
html_title:           "Bash: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (なんのため？なぜ必要？)
現在の日付取得とは、今日の年、月、日をプログラムで取り出すことです。この情報はログ、レポート生成、または特定の日付までのカウントダウンなど、多様なシナリオで重宝されます。

## How to: (やり方)
C#で現在の日付を取得するには、`DateTime`クラスを使います。以下の例をご覧ください。

```c#
using System;

class Program
{
    static void Main()
    {
        DateTime currentDate = DateTime.Now;
        Console.WriteLine(currentDate.ToString("yyyy/MM/dd"));
    }
}
```

このコードを実行すると、例えば以下のような出力が得られます。

```
2023/04/05
```

## Deep Dive (深掘り)
`DateTime.Now`は.NETの初期バージョンから存在し、日付と時刻を取得する標準的な方法です。タイムゾーンはローカルシステムの設定に依存します。代替手段として`DateTime.UtcNow`があり、これは協定世界時（UTC）を取得します。

内部的には、`DateTime`クラスは64ビットの値で日付と時刻を表現しており、0001年1月1日からのミリ秒単位で計算されています。Dateの精度は100ナノ秒です。

また、`DateTimeOffset`というクラスもあり、オフセットを含む日時情報を管理することができます。これを使用すると、タイムゾーンの違いをより正確に扱うことが可能です。

## See Also (関連情報)
- [DateTime構造体 (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- [日付と時刻のデータ型とツール (.NET documentation)](https://docs.microsoft.com/en-us/dotnet/standard/datetime/)
- [DateTimeとDateTimeOffsetを使い分ける (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/standard/datetime/choosing-between-datetime)