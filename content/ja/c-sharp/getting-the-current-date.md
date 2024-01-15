---
title:                "現在の日付の取得"
html_title:           "C#: 現在の日付の取得"
simple_title:         "現在の日付の取得"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ
日付を取得するのに役立つ簡単な方法は、プログラマーにとって絶対に必要なものです。現在の日付を取得して処理することにより、アプリケーションを柔軟に設計し、ユーザーにとって便利な体験を提供することができます。

## 使い方
まず、日付を取得するには `DateTime` 型の `Now` プロパティを使用します。以下に例を示します。

```C#
DateTime currentDate = DateTime.Now;
Console.WriteLine(currentDate);
```

このコードを実行すると、以下のような出力が得られます。

```
2022/01/01 12:00:00 AM
```

また、出力形式を自由に変更することもできます。例えば、年月日のみを表示したい場合は `ToString()` メソッドを使用します。

```C#
DateTime currentDate = DateTime.Now;
string formattedDate = currentDate.ToString("yyyy/MM/dd");
Console.WriteLine(formattedDate);
```

このコードを実行すると、以下のような出力が得られます。

```
2022/01/01
```

さらに、特定のタイムゾーンやカレンダーに基づいた日付を取得することもできます。詳細な情報は、[公式ドキュメント](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)を参照してください。

## 深堀り
`DateTime.Now` プロパティで取得する日付は、ローカルマシンのシステム時刻を基準としています。これに対し、`DateTime.UtcNow` プロパティはグリニッジ標準時を基準として日付を取得します。このような違いを理解することで、アプリケーションがどのタイムゾーンのユーザーに対しても正確な日付を表示できるようになります。

さらに、`DateTime` 型は不変性を持つため、日付の計算や比較を行う際には新しい `DateTime` オブジェクトを生成する必要があります。これについても公式ドキュメントを参照することをお勧めします。

## 併せて読みたい
- [DateTime.Now プロパティ (Microsoft Docs)](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime.now)
- [DateTime クラス (Microsoft Docs)](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime)
- [C# 日付のフォーマット (Microsoft Docs)](https://docs.microsoft.com/ja-jp/dotnet/standard/base-types/custom-date-and-time-format-strings)