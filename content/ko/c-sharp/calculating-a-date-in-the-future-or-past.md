---
title:                "미래 또는 과거 날짜 계산하기"
html_title:           "C#: 미래 또는 과거 날짜 계산하기"
simple_title:         "미래 또는 과거 날짜 계산하기"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

[y] 안녕하세요! 프로그래밍 세상에 오신걸 환영합니다! 
오늘 우리는 한가지 흥미로운 주제에 대해 이야기해볼 것인데요, 바로 "특정 날짜를 미래 또는 과거로 계산하는 방법"입니다.

## What & Why?
이 기능은 프로그래밍에서 자주 사용되는 기능 중 하나입니다. 우리가 현재 날짜를 기준으로 다음주, 다음달 또는 과거의 날짜를 계산해야 할 때, 이 기능이 사용됩니다. 이를 통해 날짜와 시간을 다루는 프로그래밍이 더욱 쉬워집니다.

## How to:
이제 코드를 통해 어떻게 날짜를 계산하는지 알아보겠습니다. 아래의 코드를 참고해주세요.

```
// 현재 날짜를 가져오기
DateTime currentDate = DateTime.Today;

// 10일 후의 날짜 계산하기
DateTime futureDate = currentDate.AddDays(10); 

// 2달 전의 날짜 계산하기
DateTime pastDate = currentDate.AddMonths(-2); 

// 결과 출력하기
Console.WriteLine("현재 날짜: " + currentDate);
Console.WriteLine("10일 후의 날짜: " + futureDate);
Console.WriteLine("2달 전의 날짜: " + pastDate);
```
코드를 실행하면 다음과 같은 결과가 나옵니다.

```
현재 날짜: 2021-10-16
10일 후의 날짜: 2021-10-26
2달 전의 날짜: 2021-08-16
```

위의 코드에서 `AddDays()` 메소드는 현재 날짜에 일수를 더해주는 기능을 합니다. 마찬가지로 `AddMonths()` 메소드는 현재 날짜에 월 수를 더하거나 빼는 기능을 합니다.

## Deep Dive:
특정 날짜를 계산하는 기능은 프로그래밍이 발전하면서 필요성을 느끼게 되었습니다. 옛날에는 날짜와 시간을 다루는 것이 매우 어려웠기 때문입니다. 하지만 지금은 .NET 프레임워크의 도움을 받아 쉽게 다룰 수 있습니다. 다른 언어에서도 비슷한 기능을 사용할 수 있으니 참고하시면 좋을 것 같습니다.

지금까지 우리는 날짜를 계산하는 방법에 대해 알아보았는데요, 만약 날짜를 비교하고 계산하는 것을 좀 더 자세하게 알고 싶다면 아래의 링크를 참고해주세요.

## See Also:
- [Microsoft Docs - DateTime 구조체](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime?view=net-5.0)
- [C# .NET 날짜와 시간 다루기](https://www.c-sharpcorner.com/article/c-sharp-datetime-manipulation/)