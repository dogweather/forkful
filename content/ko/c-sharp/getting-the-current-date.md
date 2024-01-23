---
title:                "현재 날짜 가져오기"
date:                  2024-01-20T15:13:47.561772-07:00
html_title:           "Bash: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
C#에서 현재 날짜를 가져오는 것은 DateTime 객체를 사용해서 시스템의 현재 날짜와 시간을 얻는 과정입니다. 이 정보는 로깅, 사용자 인터페이스, 날짜 계산에서 중요하게 사용됩니다.

## How to: (어떻게 하나요?)
C#에서 현재 날짜를 얻는 방법은 간단합니다. 다음 예시를 확인해보세요.

```C#
using System;

class Program
{
    static void Main()
    {
        DateTime currentDate = DateTime.Now;
        Console.WriteLine(currentDate.ToString("yyyy-MM-dd HH:mm:ss"));
        
        // 오직 날짜만.
        DateTime justDate = currentDate.Date;
        Console.WriteLine(justDate.ToString("yyyy-MM-dd"));
        
        // 오직 시간만.
        TimeSpan currentTime = currentDate.TimeOfDay;
        Console.WriteLine(currentTime.ToString("hh\\:mm\\:ss"));
    }
}
```

출력:
```
2023-03-15 15:42:10
2023-03-15
15:42:10
```

## Deep Dive (심층 분석)
`DateTime.Now`는 .NET이 시작될 때부터 있었습니다. 또 다른 옵션은 `DateTime.UtcNow`로, UTC 시간을 가져옵니다. `DateTime`은 날짜와 시간 모두를 가지고 있는데, 날짜만 필요하면 `.Date` 프로퍼티를, 시간만 필요하면 `.TimeOfDay` 프로퍼티를 사용할 수 있습니다.

시간대를 다룰 때 `DateTimeOffset`를 사용하는 것이 좋습니다. 이 타입은 시간대 오프셋 정보를 포함하고 있어서 더욱 정확한 날짜/시간 표현을 가능하게 합니다. `DateTime` 타입은 시간대를 알 수 없으므로 `DateTimeOffset`이 선호되는 경우가 많습니다.

성능에 민감한 상황에서 `DateTime.UtcNow`가 `DateTime.Now`보다 빠르다는 점도 알아둘 것입니다. `DateTime.Now`는 `DateTime.UtcNow`에 시간대 변환을 추가로 수행하기 때문입니다.

## See Also (참고 자료)
- [`DateTime` Class Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-7.0)
- [Choosing between DateTime, DateTimeOffset, TimeSpan, and TimeZoneInfo](https://docs.microsoft.com/en-us/dotnet/standard/datetime/choosing-between-datetime)
- [Formatting Date and Time for .NET](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
