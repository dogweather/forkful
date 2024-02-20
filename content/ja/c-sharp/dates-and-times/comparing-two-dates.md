---
date: 2024-01-20 17:32:53.045255-07:00
description: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B\u3068\u306F\u3001\u4E8C\u3064\
  \u306E\u65E5\u4ED8\u304C\u540C\u3058\u304B\u3069\u3046\u304B\u3001\u307E\u305F\u306F\
  \u3069\u3061\u3089\u304C\u5148\u304B\u5F8C\u304B\u3092\u5224\u65AD\u3059\u308B\u3053\
  \u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30B9\u30B1\u30B8\
  \u30E5\u30FC\u30EB\u7BA1\u7406\u3001\u671F\u9650\u306E\u78BA\u8A8D\u3001\u307E\u305F\
  \u306F\u30A8\u30F3\u30C8\u30EA\u30FC\u306E\u4E26\u3079\u66FF\u3048\u306A\u3069\u3067\
  \u65E5\u4ED8\u3092\u6BD4\u8F03\u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:01.288354
model: gpt-4-1106-preview
summary: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B\u3068\u306F\u3001\u4E8C\u3064\
  \u306E\u65E5\u4ED8\u304C\u540C\u3058\u304B\u3069\u3046\u304B\u3001\u307E\u305F\u306F\
  \u3069\u3061\u3089\u304C\u5148\u304B\u5F8C\u304B\u3092\u5224\u65AD\u3059\u308B\u3053\
  \u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30B9\u30B1\u30B8\
  \u30E5\u30FC\u30EB\u7BA1\u7406\u3001\u671F\u9650\u306E\u78BA\u8A8D\u3001\u307E\u305F\
  \u306F\u30A8\u30F3\u30C8\u30EA\u30FC\u306E\u4E26\u3079\u66FF\u3048\u306A\u3069\u3067\
  \u65E5\u4ED8\u3092\u6BD4\u8F03\u3057\u307E\u3059\u3002"
title: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
日付を比較するとは、二つの日付が同じかどうか、またはどちらが先か後かを判断することです。プログラマーはスケジュール管理、期限の確認、またはエントリーの並べ替えなどで日付を比較します。

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
