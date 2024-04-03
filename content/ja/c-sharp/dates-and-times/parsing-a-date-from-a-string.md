---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:50.730137-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.136978-06:00'
model: gpt-4-0125-preview
summary: "C#\u3067\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\
  \u308B\u3053\u3068\u306F\u3001\u65E5\u4ED8\u3068\u6642\u523B\u306E\u30C6\u30AD\u30B9\
  \u30C8\u8868\u73FE\u3092`DateTime`\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u306B\u5909\
  \u63DB\u3059\u308B\u3053\u3068\u3092\u542B\u307F\u307E\u3059\u3002\u3053\u308C\u306F\
  \u3001\u4E88\u5B9A\u30A2\u30D7\u30EA\u3001\u30ED\u30B0\u30D7\u30ED\u30BB\u30C3\u30B5\
  \u30FC\u3001\u307E\u305F\u306F\u30E6\u30FC\u30B6\u30FC\u3084\u5916\u90E8\u30BD\u30FC\
  \u30B9\u304B\u3089\u306E\u65E5\u4ED8\u5165\u529B\u3092\u51E6\u7406\u3059\u308B\u30B7\
  \u30B9\u30C6\u30E0\u306A\u3069\u3001\u3055\u307E\u3056\u307E\u306A\u5F62\u5F0F\u3067\
  \u65E5\u4ED8\u3068\u6642\u523B\u3092\u64CD\u4F5C\u3001\u4FDD\u5B58\u3001\u307E\u305F\
  \u306F\u8868\u793A\u3059\u308B\u5FC5\u8981\u304C\u3042\u308B\u30A2\u30D7\u30EA\u30B1\
  \u30FC\u30B7\u30E7\u30F3\u306B\u3068\u3063\u3066\u4E0D\u53EF\u6B20\u3067\u3059\u3002\
  ."
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
weight: 30
---

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
