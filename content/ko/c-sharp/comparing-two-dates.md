---
title:                "두 날짜 비교하기"
html_title:           "C#: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 필요한가?
두 날짜를 비교하는 것은 두 시점 간의 차이를 확인하는 프로세스입니다. 프로그래머는 기한, 날짜 범위, 시간 경과 감지등의 사항을 처리하기 위해 이를 사용합니다.

## 방법:
두 날짜를 비교하는 기본적인 C# 코드를 확인해보겠습니다.

```C#
DateTime date1 = new DateTime(2022, 1, 1);
DateTime date2 = new DateTime(2022, 12, 31);

int result = DateTime.Compare(date1, date2);

if (result < 0)
    Console.WriteLine("date1 is earlier than date2.");
else if (result == 0)
    Console.WriteLine("date1 is the same as date2.");
else
    Console.WriteLine("date1 is later than date2.");
```
출력 결과는 다음과 같습니다.
```C#
date1 is earlier than date2.
```
## 디프 다이브:
두 날짜를 비교하는 것은 프로그래밍의 역사와 맞물려 있습니다. 두 시간 이벤트의 순서를 추적하는 것은 유용한 자료구조 및 알고리즘을 제공합니다. 

이에 대한 대안으로는 TimeSpan 구조를 활용하는 것이 있습니다. TimeSpan은 두 날짜 간의 시간 차이를 나타내고 있어, 일정 범위 또는 시간 이후의 일정을 계산하기에 적합합니다.

```C#
DateTime date1 = new DateTime(2022, 1, 1);
DateTime date2 = new DateTime(2022, 12, 31);

TimeSpan difference = date2.Subtract(date1);

Console.WriteLine("Difference in days: " + difference.Days);
```

또한, 타임존의 영향으로 날짜 비교에 있어서는 DateTime의 Utc 속성을 이용하여 UTC 시간으로 비교하는 것이 안전합니다.

## 참고 자료:
1. [Microsoft Official Documentation: DateTime Structure](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
2. [Microsoft Official Documentation: TimeSpan Structure](https://docs.microsoft.com/en-us/dotnet/api/system.timespan?view=net-5.0)