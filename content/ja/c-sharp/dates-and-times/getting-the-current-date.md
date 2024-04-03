---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:22.513996-07:00
description: "\u3069\u306E\u3088\u3046\u306B\uFF1A C#\u306F\u3001.NET Framework\u306E\
  System\u540D\u524D\u7A7A\u9593\u306E\u4E00\u90E8\u3067\u3042\u308B`DateTime`\u30AF\
  \u30E9\u30B9\u3092\u4F7F\u7528\u3057\u3066\u3001\u73FE\u5728\u306E\u65E5\u4ED8\u3092\
  \u53D6\u5F97\u3059\u308B\u305F\u3081\u306E\u7C21\u5358\u306A\u65B9\u6CD5\u3092\u63D0\
  \u4F9B\u3057\u307E\u3059\u3002\u4EE5\u4E0B\u306E\u4F8B\u306F\u3001\u73FE\u5728\u306E\
  \u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\u65B9\u6CD5\u3001\u304A\u3088\u3073\u30AA\
  \u30D7\u30B7\u30E7\u30F3\u3067\u3001\u6642\u523B\u3092\u53D6\u5F97\u3059\u308B\u65B9\
  \u6CD5\u3092\u793A\u3057\u3066\u3044\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:42.137976-06:00'
model: gpt-4-0125-preview
summary: "C#\u306F\u3001.NET Framework\u306ESystem\u540D\u524D\u7A7A\u9593\u306E\u4E00\
  \u90E8\u3067\u3042\u308B`DateTime`\u30AF\u30E9\u30B9\u3092\u4F7F\u7528\u3057\u3066\
  \u3001\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\u305F\u3081\u306E\
  \u7C21\u5358\u306A\u65B9\u6CD5\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\u4EE5\u4E0B\
  \u306E\u4F8B\u306F\u3001\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\
  \u65B9\u6CD5\u3001\u304A\u3088\u3073\u30AA\u30D7\u30B7\u30E7\u30F3\u3067\u3001\u6642\
  \u523B\u3092\u53D6\u5F97\u3059\u308B\u65B9\u6CD5\u3092\u793A\u3057\u3066\u3044\u307E\
  \u3059."
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

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
