---
date: 2024-01-20 17:28:39.552079-07:00
description: "How to: (\uBC29\uBC95) .NET\uC5D0\uC11C \uB0A0\uC9DC \uACC4\uC0B0\uC740\
  \ DateTime \uAD6C\uC870\uCCB4\uB97C \uD1B5\uD574 \uC218\uD589\uB429\uB2C8\uB2E4\
  . 2002\uB144 .NET\uC758 \uCCAB \uBC84\uC804 \uBC1C\uD45C \uC774\uD6C4, \uC774 \uAD6C\
  \uC870\uCCB4\uB294 \uB0A0\uC9DC\uC640 \uC2DC\uAC04\uACFC \uAD00\uB828\uB41C \uC5F0\
  \uC0B0\uC744 \uC27D\uAC8C \uB9CC\uB4E4\uC5C8\uC2B5\uB2C8\uB2E4. `AddDays` \uBA54\
  \uC11C\uB4DC \uC678\uC5D0\uB3C4 `AddMonths`, `AddYears` \uAC19\uC740\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:56.976443-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) .NET\uC5D0\uC11C \uB0A0\uC9DC \uACC4\uC0B0\uC740 DateTime\
  \ \uAD6C\uC870\uCCB4\uB97C \uD1B5\uD574 \uC218\uD589\uB429\uB2C8\uB2E4."
title: "\uBBF8\uB798 \uD639\uC740 \uACFC\uAC70\uC758 \uB0A0\uC9DC \uACC4\uC0B0\uD558\
  \uAE30"
weight: 26
---

## How to: (방법)
```C#
using System;

public class DateCalculation
{
    public static void Main()
    {
        DateTime today = DateTime.Now;
        DateTime futureDate = today.AddDays(10); // 10일 후
        DateTime pastDate = today.AddDays(-15); // 15일 전

        Console.WriteLine($"오늘 날짜: {today.ToShortDateString()}");
        Console.WriteLine($"미래의 날짜: {futureDate.ToShortDateString()}");
        Console.WriteLine($"과거의 날짜: {pastDate.ToShortDateString()}");
    }
}
```

샘플 출력:
```
오늘 날짜: 03/15/2023
미래의 날짜: 03/25/2023
과거의 날짜: 02/28/2023
```

## Deep Dive (심층 분석)
.NET에서 날짜 계산은 DateTime 구조체를 통해 수행됩니다. 2002년 .NET의 첫 버전 발표 이후, 이 구조체는 날짜와 시간과 관련된 연산을 쉽게 만들었습니다. `AddDays` 메서드 외에도 `AddMonths`, `AddYears` 같은 메서드로 다양한 계산이 가능합니다.

역사적으로, 날짜 계산은 해, 달, 일의 변환을 필요로 했었고 불규칙한 달력 시스템 때문에 복잡했습니다. 그러나 DateTime 구조체는 그레고리력만을 기준으로 계산을 단순화 해 줍니다. 또한, DateTime은 시간대 변환을 고려하지 않습니다. 이럴 때는 `DateTimeOffset`이나 `TimeZoneInfo` 클래스를 사용할 수 있습니다.

해당 방법의 대안으로는 NodaTime 같은 외부 라이브러리도 존재합니다. 이러한 라이브러리는 날짜 및 시간 계산을 더욱 강력하게 처리할 수 있도록 다양한 기능들을 제공합니다.

날짜를 더하는 것 외에도, 날짜를 비교하거나 특정 요일을 찾는 등의 복잡한 날짜 연산을 할 수 있습니다. `DateTime.Compare`, `DateTime.DayOfWeek` 같은 메서드를 이용합니다. 이 모든 기능은 복잡한 날짜 관리를 단순화하여 프로그래머의 수고를 덜어줍니다.

## See Also (참고 자료)
- Microsoft .NET Documentation for DateTime: [https://docs.microsoft.com/dotnet/api/system.datetime](https://docs.microsoft.com/dotnet/api/system.datetime)
- NodaTime - A better date and time API for .NET: [https://nodatime.org/](https://nodatime.org/)
