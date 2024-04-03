---
date: 2024-01-20 17:32:53.045255-07:00
description: "How to: (\u65B9\u6CD5) C#\u3067\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B\
  \u306E\u306F\u7C21\u5358\u3067\u3059\u3002DateTime\u30AA\u30D6\u30B8\u30A7\u30AF\
  \u30C8\u3092\u4F7F\u7528\u3057\u3066\u3001\u4EE5\u4E0B\u306E\u3088\u3046\u306B\u6BD4\
  \u8F03\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.140982-06:00'
model: gpt-4-1106-preview
summary: "C#\u3067\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B\u306E\u306F\u7C21\u5358\
  \u3067\u3059\u3002DateTime\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u3092\u4F7F\u7528\
  \u3057\u3066\u3001\u4EE5\u4E0B\u306E\u3088\u3046\u306B\u6BD4\u8F03\u3092\u884C\u3044\
  \u307E\u3059."
title: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B"
weight: 27
---

## How to: (方法)
C#で日付を比較するのは簡単です。DateTimeオブジェクトを使用して、以下のように比較を行います。

```csharp
DateTime firstDate = new DateTime(2023, 3, 1);
DateTime secondDate = new DateTime(2023, 3, 15);

// 比較: 同じか?
bool areDatesEqual = firstDate == secondDate;

// 比較: firstDateはsecondDateより前か?
bool isFirstBeforeSecond = firstDate < secondDate;

// 比較: firstDateはsecondDateより後か?
bool isFirstAfterSecond = firstDate > secondDate;

Console.WriteLine($"Dates equal: {areDatesEqual}");
Console.WriteLine($"First date is before second: {isFirstBeforeSecond}");
Console.WriteLine($"First date is after second: {isFirstAfterSecond}");
```

実行結果:
```
Dates equal: False
First date is before second: True
First date is after second: False
```

## Deep Dive (深掘り)
C#における日付の比較は、DateTime構造によって支えられています。これは、2002年の.NET Framework 1.0リリース以来利用されています。DateOnlyやTimeOnlyなど、日付のみや時間のみを扱う代替の型もあります。

DateTime.CompareToメソッドやDateTime.Equalsメソッド等、他の比較方法もあります。内部実装では、DateTimeは64ビットの値で表され、それは1970年1月1日からのミリ秒を示しています。このため、比較は数値比較として高速に行うことができます。

## See Also (関連情報)
- Microsoftの公式ドキュメント: [DateTime Struct](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- 比較メソッドについてもっと学ぶ: [DateTime.CompareTo Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compareto?view=net-6.0)
