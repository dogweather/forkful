---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:22.513996-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:42.137976-06:00'
model: gpt-4-0125-preview
summary: "C#\u3067\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\u3053\
  \u3068\u306F\u3001\u30B7\u30B9\u30C6\u30E0\u304B\u3089\u73FE\u5728\u306E\u65E5\u4ED8\
  \u3068\u6642\u9593\u306E\u8A73\u7D30\u3092\u53D6\u5F97\u3059\u308B\u3053\u3068\u3092\
  \u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\
  \u30A2\u30AF\u30B7\u30E7\u30F3\u304C\u6B63\u78BA\u306B\u30BF\u30A4\u30DF\u30F3\u30B0\
  \u3055\u308C\u3001\u30C7\u30FC\u30BF\u306B\u6B63\u78BA\u306A\u30BF\u30A4\u30E0\u30B9\
  \u30BF\u30F3\u30D7\u304C\u4ED8\u3051\u3089\u308C\u308B\u3088\u3046\u306B\u3001\u30ED\
  \u30B0\u8A18\u9332\u3001\u30BF\u30A4\u30E0\u30B9\u30BF\u30F3\u30D4\u30F3\u30B0\u64CD\
  \u4F5C\u3001\u307E\u305F\u306F\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u5185\
  \u306E\u30BF\u30B9\u30AF\u306E\u30B9\u30B1\u30B8\u30E5\u30FC\u30EA\u30F3\u30B0\u306A\
  \u3069\u3067\u3053\u306E\u60C5\u5831\u306B\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u5FC5\
  \u8981\u304C\u3088\u304F\u3042\u308A\u307E\u3059\u3002."
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

## 何となぜ？
C#で現在の日付を取得することは、システムから現在の日付と時間の詳細を取得することを意味します。プログラマーは、アクションが正確にタイミングされ、データに正確なタイムスタンプが付けられるように、ログ記録、タイムスタンピング操作、またはアプリケーション内のタスクのスケジューリングなどでこの情報にアクセスする必要がよくあります。

## どのように：
C#は、.NET FrameworkのSystem名前空間の一部である`DateTime`クラスを使用して、現在の日付を取得するための簡単な方法を提供します。以下の例は、現在の日付を取得する方法、およびオプションで、時刻を取得する方法を示しています。

```csharp
using System;

class Program
{
    static void Main()
    {
        // 現在の日付のみを取得
        DateTime currentDate = DateTime.Today;
        Console.WriteLine(currentDate.ToString("d"));  // 出力: MM/dd/yyyy
        
        // 現在の日付と時刻を取得
        DateTime currentDateTime = DateTime.Now;
        Console.WriteLine(currentDateTime.ToString()); // 出力: MM/dd/yyyy HH:mm:ss

        // 現在のUTC日付と時刻を取得
        DateTime currentUtcDateTime = DateTime.UtcNow;
        Console.WriteLine(currentUtcDateTime.ToString()); // 出力: MM/dd/yyyy HH:mm:ss
    }
}
```

サードパーティのライブラリに関しては、NodaTimeは異なるカレンダーやタイムゾーンで現在の日付を取得することを含む、日付と時刻の操作のための堅牢な代替手段を提供します。

```csharp
using NodaTime;
using System;

class Program
{
    static void Main()
    {
        // NodaTimeを使用してISOカレンダーで現在の日付を取得
        LocalDate currentDate = SystemClock.Instance.GetCurrentInstant().InUtc().Date;
        Console.WriteLine(currentDate.ToString()); // 出力: yyyy-MM-dd

        // タイムゾーン固有の日付のために
        DateTimeZone zone = DateTimeZoneProviders.Tzdb["America/New_York"];
        LocalDate currentZonedDate = SystemClock.Instance.GetCurrentInstant().InZone(zone).Date;
        Console.WriteLine(currentZonedDate.ToString()); // 出力: yyyy-MM-dd
    }
}
```

これは、組み込みの`DateTime`クラスと、異なるタイムゾーンまたはカレンダーシステムを扱う必要があるアプリケーションに特に便利なNodaTimeによって提供される拡張機能の基本的な使用方法を示しています。
