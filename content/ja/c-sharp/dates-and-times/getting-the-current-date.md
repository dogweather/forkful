---
title:                "現在の日付の取得"
aliases:
- /ja/c-sharp/getting-the-current-date/
date:                  2024-02-03T19:09:22.513996-07:00
model:                 gpt-4-0125-preview
simple_title:         "現在の日付の取得"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/c-sharp/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
