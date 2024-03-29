---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:50.730137-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.136978-06:00'
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？
C#で文字列から日付を解析することは、日付と時刻のテキスト表現を`DateTime`オブジェクトに変換することを含みます。これは、予定アプリ、ログプロセッサー、またはユーザーや外部ソースからの日付入力を処理するシステムなど、さまざまな形式で日付と時刻を操作、保存、または表示する必要があるアプリケーションにとって不可欠です。

## どのようにして：

**基本的な解析：**

文字列を`DateTime`に変換するには、`DateTime.Parse`および`DateTime.TryParse`メソッドが最適なオプションです。こちらが簡単な例です：

```csharp
string dateString = "2023-04-12";
DateTime parsedDate;

if (DateTime.TryParse(dateString, out parsedDate))
{
    Console.WriteLine($"正常に解析されました: {parsedDate}");
}
else
{
    Console.WriteLine("解析に失敗しました。");
}
// 出力：正常に解析されました: 2023/04/12 0:00:00
```

**カルチャを指定する：**

時には、特定のカルチャ形式の日付文字列を解析する必要があります。これは`CultureInfo`クラスを使用して達成できます：

```csharp
using System.Globalization;

string dateString = "12 avril 2023";
var cultureInfo = new CultureInfo("fr-FR");
DateTime parsedDate = DateTime.Parse(dateString, cultureInfo);

Console.WriteLine(parsedDate);
// 出力：2023/04/12 0:00:00
```

**特定の形式での正確な解析：**

標準ではない可能性がある特定の形式で日付が提供されるシナリオでは、`DateTime.ParseExact`が便利です：

```csharp
string dateString = "Wednesday, 12 April 2023";
string format = "dddd, d MMMM yyyy";
DateTime parsedDate = DateTime.ParseExact(dateString, format, CultureInfo.InvariantCulture);

Console.WriteLine(parsedDate);
// 出力：2023/04/12 0:00:00
```

**NodaTimeを使用する：**

さらに堅牢な日付と時刻の解析には、人気のサードパーティライブラリであるNodaTimeの使用を検討してください。これは、より広範な日付/時刻処理機能を提供します：

```csharp
using NodaTime;
using NodaTime.Text;

var pattern = LocalDatePattern.CreateWithInvariantCulture("yyyy-MM-dd");
var parseResult = pattern.Parse("2023-04-12");

if (parseResult.Success)
{
    LocalDate localDate = parseResult.Value;
    Console.WriteLine(localDate); // 2023-04-12
}
else
{
    Console.WriteLine("解析に失敗しました。");
}
```

NodaTimeは、タイムゾーン、期間と持続期間の概念、さまざまなカレンダーシステムのサポートを幅広く提供し、.NETアプリケーションでの複雑な日付と時刻の操作に強力な選択肢となります。
