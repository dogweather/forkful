---
date: 2024-01-20 17:31:13.372112-07:00
description: "How to: (\u65B9\u6CD5) C#\u3067\u306F`DateTime`\u30AF\u30E9\u30B9\u3092\
  \u4F7F\u3063\u3066\u65E5\u4ED8\u8A08\u7B97\u3092\u3057\u307E\u3059\u3002\u6570\u65E5\
  \u5F8C\u3084\u6570\u65E5\u524D\u306E\u65E5\u4ED8\u3092\u7C21\u5358\u306B\u6C42\u3081\
  \u3089\u308C\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.014963-06:00'
model: gpt-4-1106-preview
summary: "(\u65B9\u6CD5) C#\u3067\u306F`DateTime`\u30AF\u30E9\u30B9\u3092\u4F7F\u3063\
  \u3066\u65E5\u4ED8\u8A08\u7B97\u3092\u3057\u307E\u3059\u3002\u6570\u65E5\u5F8C\u3084\
  \u6570\u65E5\u524D\u306E\u65E5\u4ED8\u3092\u7C21\u5358\u306B\u6C42\u3081\u3089\u308C\
  \u307E\u3059\u3002"
title: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\
  \u3059\u308B"
weight: 26
---

## How to: (方法)
C#では`DateTime`クラスを使って日付計算をします。数日後や数日前の日付を簡単に求められます。

```C#
using System;

class Program
{
    static void Main()
    {
        DateTime today = DateTime.Now;
        
        // 10日後の日付を計算
        DateTime futureDate = today.AddDays(10);
        Console.WriteLine($"10日後: {futureDate.ToShortDateString()}");

        // 5日前の日付を計算
        DateTime pastDate = today.AddDays(-5);
        Console.WriteLine($"5日前: {pastDate.ToShortDateString()}");
    }
}
```

実行結果 (日付は変わります):

```
10日後: 04/15/2023
5日前: 03/31/2023
```

## Deep Dive (深掘り)
日付計算は、C#が初めて登場した2000年代から利用されています。標準ライブラリーの`System.DateTime`は、日付と時間を表し、操作する機能を提供します。

代替方法として`TimeSpan`を使うこともできますが、日数計算には`DateTime.AddDays`メソッドが便利です。また、タイムゾーンを意識した計算が必要な場合は`DateTimeOffset`を使うことを検討してください。

実装の詳細については、`DateTime`のメソッドはうるう年や月末の日数に対応しているため、独自の計算ロジックを書く必要がありません。ただし、夏時間など特別なケースでは追加の検討が必要です。

## See Also (関連情報)
- [DateTimeクラス (Microsoft Docs)](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetime?view=net-6.0)
- [TimeSpanクラス (Microsoft Docs)](https://docs.microsoft.com/ja-jp/dotnet/api/system.timespan?view=net-6.0)
- [DateTimeOffsetクラス (Microsoft Docs)](https://docs.microsoft.com/ja-jp/dotnet/api/system.datetimeoffset?view=net-6.0)
