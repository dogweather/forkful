---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:19.426333-07:00
description: "\uC5B4\uB5BB\uAC8C: C#\uC740 .NET Framework\uC758 System \uB124\uC784\
  \uC2A4\uD398\uC774\uC2A4 \uC77C\uBD80\uC778 `DateTime` \uD074\uB798\uC2A4\uB97C\
  \ \uC0AC\uC6A9\uD558\uC5EC \uD604\uC7AC \uB0A0\uC9DC\uB97C \uAC00\uC838\uC624\uB294\
  \ \uAC04\uB2E8\uD55C \uBC29\uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4. \uC544\uB798\
  \ \uC608\uC81C\uB294 \uD604\uC7AC \uB0A0\uC9DC\uB97C \uAC00\uC838\uC624\uB294 \uBC29\
  \uBC95\uC744 \uBCF4\uC5EC\uC8FC\uBA70, \uC120\uD0DD\uC801\uC73C\uB85C \uC2DC\uAC04\
  \uB3C4 \uD568\uAED8 \uAC00\uC838\uC635\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.249927-06:00'
model: gpt-4-0125-preview
summary: "C#\uC740 .NET Framework\uC758 System \uB124\uC784\uC2A4\uD398\uC774\uC2A4\
  \ \uC77C\uBD80\uC778 `DateTime` \uD074\uB798\uC2A4\uB97C \uC0AC\uC6A9\uD558\uC5EC\
  \ \uD604\uC7AC \uB0A0\uC9DC\uB97C \uAC00\uC838\uC624\uB294 \uAC04\uB2E8\uD55C \uBC29\
  \uBC95\uC744 \uC81C\uACF5\uD569\uB2C8\uB2E4."
title: "\uD604\uC7AC \uB0A0\uC9DC \uAC00\uC838\uC624\uAE30"
weight: 29
---

## 어떻게:
C#은 .NET Framework의 System 네임스페이스 일부인 `DateTime` 클래스를 사용하여 현재 날짜를 가져오는 간단한 방법을 제공합니다. 아래 예제는 현재 날짜를 가져오는 방법을 보여주며, 선택적으로 시간도 함께 가져옵니다.

```csharp
using System;

class Program
{
    static void Main()
    {
        // 현재 날짜만 가져옴
        DateTime currentDate = DateTime.Today;
        Console.WriteLine(currentDate.ToString("d"));  // 출력: MM/dd/yyyy
        
        // 현재 날짜 및 시간을 가져옴
        DateTime currentDateTime = DateTime.Now;
        Console.WriteLine(currentDateTime.ToString()); // 출력: MM/dd/yyyy HH:mm:ss

        // 현재 UTC 날짜 및 시간을 가져옴
        DateTime currentUtcDateTime = DateTime.UtcNow;
        Console.WriteLine(currentUtcDateTime.ToString()); // 출력: MM/dd/yyyy HH:mm:ss
    }
}
```

제 3자 라이브러리에 관해서는, NodaTime은 다양한 달력과 타임존에서 현재 날짜를 가져올 수 있도록 하는 강력한 대안을 제공합니다.

```csharp
using NodaTime;
using System;

class Program
{
    static void Main()
    {
        // ISO 달력에서 현재 날짜를 가져오기 위해 NodaTime 사용
        LocalDate currentDate = SystemClock.Instance.GetCurrentInstant().InUtc().Date;
        Console.WriteLine(currentDate.ToString()); // 출력: yyyy-MM-dd

        // 타임존 특정 날짜를 위해
        DateTimeZone zone = DateTimeZoneProviders.Tzdb["America/New_York"];
        LocalDate currentZonedDate = SystemClock.Instance.GetCurrentInstant().InZone(zone).Date;
        Console.WriteLine(currentZonedDate.ToString()); // 출력: yyyy-MM-dd
    }
}
```

이는 내장된 `DateTime` 클래스의 기본 사용과 다른 타임존 또는 달력 시스템을 처리할 필요가 있는 애플리케이션에 특히 유용한 NodaTime에 의해 제공되는 향상된 기능을 보여줍니다.
