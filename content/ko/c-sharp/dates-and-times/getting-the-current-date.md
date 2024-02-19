---
aliases:
- /ko/c-sharp/getting-the-current-date/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:19.426333-07:00
description: "C#\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\uB97C \uAC00\uC838\uC624\uB294\
  \ \uAC83\uC740 \uC2DC\uC2A4\uD15C\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\uC640 \uC2DC\
  \uAC04 \uC138\uBD80 \uC815\uBCF4\uB97C \uAC00\uC838\uC624\uB294 \uAC83\uC744 \uD3EC\
  \uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC885\uC885\
  \ \uC774 \uC815\uBCF4\uC5D0 \uC561\uC138\uC2A4\uD574\uC57C \uD560 \uD544\uC694\uAC00\
  \ \uC788\uB294\uB370, \uC774\uB294 \uB85C\uAE45, \uD0C0\uC784\uC2A4\uD0EC\uD504\
  \ \uC791\uC5C5 \uB610\uB294 \uC560\uD50C\uB9AC\uCF00\uC774\uC158 \uB0B4 \uC791\uC5C5\
  \uC744 \uC608\uC57D\uD558\uACE0, \uD589\uB3D9\uC774 \uC815\uD655\uD558\uAC8C \uC2DC\
  \uAC04\uC744 \uB9DE\uCD94\uACE0 \uB370\uC774\uD130\uAC00\u2026"
lastmod: 2024-02-18 23:09:06.236814
model: gpt-4-0125-preview
summary: "C#\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\uB97C \uAC00\uC838\uC624\uB294\
  \ \uAC83\uC740 \uC2DC\uC2A4\uD15C\uC5D0\uC11C \uD604\uC7AC \uB0A0\uC9DC\uC640 \uC2DC\
  \uAC04 \uC138\uBD80 \uC815\uBCF4\uB97C \uAC00\uC838\uC624\uB294 \uAC83\uC744 \uD3EC\
  \uD568\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC885\uC885\
  \ \uC774 \uC815\uBCF4\uC5D0 \uC561\uC138\uC2A4\uD574\uC57C \uD560 \uD544\uC694\uAC00\
  \ \uC788\uB294\uB370, \uC774\uB294 \uB85C\uAE45, \uD0C0\uC784\uC2A4\uD0EC\uD504\
  \ \uC791\uC5C5 \uB610\uB294 \uC560\uD50C\uB9AC\uCF00\uC774\uC158 \uB0B4 \uC791\uC5C5\
  \uC744 \uC608\uC57D\uD558\uACE0, \uD589\uB3D9\uC774 \uC815\uD655\uD558\uAC8C \uC2DC\
  \uAC04\uC744 \uB9DE\uCD94\uACE0 \uB370\uC774\uD130\uAC00\u2026"
title: "\uD604\uC7AC \uB0A0\uC9DC \uAC00\uC838\uC624\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?
C#에서 현재 날짜를 가져오는 것은 시스템에서 현재 날짜와 시간 세부 정보를 가져오는 것을 포함합니다. 프로그래머들은 종종 이 정보에 액세스해야 할 필요가 있는데, 이는 로깅, 타임스탬프 작업 또는 애플리케이션 내 작업을 예약하고, 행동이 정확하게 시간을 맞추고 데이터가 정확한 타임스탬프를 가지도록 보장하기 위함입니다.

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
