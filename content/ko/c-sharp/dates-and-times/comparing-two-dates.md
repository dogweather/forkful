---
date: 2024-01-20 17:32:26.116571-07:00
description: "\uB0A0\uC9DC \uB450 \uAC1C\uB97C \uBE44\uAD50\uD55C\uB2E4\uB294 \uAC83\
  \uC740 \uB0A0\uC9DC \uC2DC\uAC04 \uAC12\uB4E4\uC744 \uC11C\uB85C \uBE44\uAD50\uD574\
  \ \uACFC\uAC70, \uD604\uC7AC, \uBBF8\uB798 \uC0C1\uD0DC\uB97C \uC815\uD558\uB294\
  \ \uAC83\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC774 \uC774\uB97C\
  \ \uD558\uB294 \uC774\uC720\uB294 \uC608\uC57D \uC2DC\uC2A4\uD15C, \uB0A0\uC9DC\
  \ \uCC28\uC774 \uACC4\uC0B0, \uC720\uD6A8\uC131 \uAC80\uC0AC \uB54C\uBB38\uC785\uB2C8\
  \uB2E4."
isCJKLanguage: true
lastmod: 2024-02-19 22:05:14.163478
model: gpt-4-1106-preview
summary: "\uB0A0\uC9DC \uB450 \uAC1C\uB97C \uBE44\uAD50\uD55C\uB2E4\uB294 \uAC83\uC740\
  \ \uB0A0\uC9DC \uC2DC\uAC04 \uAC12\uB4E4\uC744 \uC11C\uB85C \uBE44\uAD50\uD574 \uACFC\
  \uAC70, \uD604\uC7AC, \uBBF8\uB798 \uC0C1\uD0DC\uB97C \uC815\uD558\uB294 \uAC83\uC785\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC774 \uC774\uB97C \uD558\uB294\
  \ \uC774\uC720\uB294 \uC608\uC57D \uC2DC\uC2A4\uD15C, \uB0A0\uC9DC \uCC28\uC774\
  \ \uACC4\uC0B0, \uC720\uD6A8\uC131 \uAC80\uC0AC \uB54C\uBB38\uC785\uB2C8\uB2E4."
title: "\uB450 \uB0A0\uC9DC \uBE44\uAD50\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
날짜 두 개를 비교한다는 것은 날짜 시간 값들을 서로 비교해 과거, 현재, 미래 상태를 정하는 것입니다. 프로그래머들이 이를 하는 이유는 예약 시스템, 날짜 차이 계산, 유효성 검사 때문입니다.

## How to: (방법)
```C#
using System;

class Program
{
    static void Main()
    {
        DateTime date1 = new DateTime(2023, 1, 1);
        DateTime date2 = DateTime.Now;

        int comparison = DateTime.Compare(date1, date2);

        if (comparison < 0)
            Console.WriteLine($"{date1:d} is earlier than {date2:d}");
        else if (comparison == 0)
            Console.WriteLine($"{date1:d} is the same as {date2:d}");
        else
            Console.WriteLine($"{date1:d} is later than {date2:d}");
    }
}
```
Sample Output:
```
2023-01-01 is earlier than 2023-04-05
```

## Deep Dive (심층 분석)
날짜 비교하기 전에는 시간대나 `DateTimeKind` 설정 등을 유념하세요. `DateTime.Compare`는 간단하고 직관적인 기본 제공 메서드입니다. 하지만 여러분 자신의 요구 사항에 따른 별도의 동작을 구현하고 싶다면 `TimeSpan` 객체 또는 `DateTimeOffset` 클래스 같은 방법을 고려할 수도 있습니다. `TimeSpan`은 날짜 간의 차이를 계산할 때 유용하고, `DateTimeOffset`은 시간대를 고려한 더 정확한 시간 비교를 할 때 활용됩니다.

## See Also (참고 자료)
- Microsoft `DateTime` Documentation: [docs.microsoft.com/dotnet/api/system.datetime](https://docs.microsoft.com/en-us/dotnet/api/system.datetime)
- Microsoft `TimeSpan` Documentation: [docs.microsoft.com/dotnet/api/system.timespan](https://docs.microsoft.com/en-us/dotnet/api/system.timespan)
- Microsoft `DateTimeOffset` Documentation: [docs.microsoft.com/dotnet/api/system.datetimeoffset](https://docs.microsoft.com/en-us/dotnet/api/system.datetimeoffset)
