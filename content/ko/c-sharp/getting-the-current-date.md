---
title:                "현재 날짜 가져오기"
html_title:           "C: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

현재 날짜를 얻는 것은 단순히 현재 시스템의 날짜 정보를 확인하는 작업을 의미합니다. 이는 특정 작업의 트래킹, 시간 종속적인 기능 구현, 혹은 패치 업데이트 등에서 도움이 됩니다.


## 어떻게 할까?

다음은 C#에서 날짜를 얻는 방법에 대한 간단한 예입니다:

```C#
using System;

class Program
{
    static void Main()
    {
        DateTime currentDate = DateTime.Now;
        Console.WriteLine("현재 날짜와 시간: " + currentDate);
    }
}
```

이 코드를 실행하면 현재 날짜와 시간을 출력합니다.


## 깊게 알아보기

현재 날짜를 얻는 것은 오래된 프로그래밍의 핵심 요소 중 하나입니다. DateTime 클래스는 .NET 1.0부터 시작해 현재까지 이용되고 있습니다. C#에서 알ternative로 `DateTimeOffset` 메소드를 사용할 수 있습니다. 이는 시간대 정보가 포함된 날짜와 시간을 제공합니다.

```C#
DateTimeOffset currentDate = DateTimeOffset.Now;
```

이것은 현재 시스템의 현지 날짜 및 시간을 반환하며, UTC와의 차이도 함께 제공합니다.


## 참고 자료

- [C#의 DateTime에 대한 Microsoft 공식 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime?view=net-5.0)
- [C#의 DateTimeOffset에 대한 Microsoft 공식 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetimeoffset?view=net-5.0)