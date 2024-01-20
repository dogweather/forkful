---
title:                "미래나 과거의 날짜 계산하기"
html_title:           "C#: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 무엇이며 왜?

데이터 계산은 특정 날짜를 기반으로 미래 또는 과거의 날짜를 찾는 것입니다. 프로그래머들은 예약시스템, 휴가 계획, 추적 시스템 등, 시간 기반 로직이 필요한 다양한 애플리케이션 개발에 이를 사용합니다. 

## 사용 방법:

기존의 DateTime 객체에 TimeSpan 객체를 더하거나 빼는 방식으로 쉽게 이용할 수 있습니다.

```C#
DateTime present = DateTime.Now;
Console.WriteLine("Today: " + present);

// Future date calculation
TimeSpan futureSpan = new TimeSpan(90, 0, 0, 0);
DateTime future = present.Add(futureSpan);
Console.WriteLine("90 days later: " + future);

// Past date calculation
TimeSpan pastSpan = new TimeSpan(90, 0, 0, 0);
DateTime past = present.Subtract(pastSpan);
Console.WriteLine("90 days back: " + past);
```

출력 예제:

```C#
Today: 4/3/2023 12:14:45 PM
90 days later: 7/2/2023 12:14:45 PM
90 days back: 1/3/2023 12:14:45 PM
```

## 상세 정보:

DateTime 및 TimeSpan은 .NET 프레임워크가 초기 출시되었을 때 (2002년)부터 사용되었습니다. 이 경우 사용자에게 훨씬 더 유연한 시간 계산 기능을 제공합니다. 그러나 기타 방법으로 Unix time을 사용하여 시간을 계산하는 것도 가능합니다. 아래에 대한 구현 예제를 제공합니다.

```C#
DateTime present_UNIX = DateTime.UtcNow;
double presentUNIXTimeStamp = (present_UNIX.Subtract(new DateTime(1970, 1, 1))).TotalSeconds;
Console.WriteLine("UNIX timestamp now: " + presentUNIXTimeStamp);
double futureUNIXTimeStamp = presentUNIXTimeStamp + (90 * 24 * 60 * 60); 
Console.WriteLine("UNIX timestamp 90 days later: " + futureUNIXTimeStamp);
double pastUNIXTimeStamp = presentUNIXTimeStamp - (90 *24 * 60 * 60);
Console.WriteLine("UNIX timestamp 90 days back: " + pastUNIXTimeStamp);
```

그러나 일반적으로 C#을 사용하는 개발자들은 간단함과 가독성 덕분에 DateTime 및 TimeSpan을 선호합니다.

## 참고 자료:

- Microsoft Official Documentation: [DateTime Struct](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime?view=net-6.0)
- Microsoft Official Documentation: [TimeSpan Struct](https://docs.microsoft.com/ko-kr/dotnet/api/system.timespan?view=net-6.0)
- StackOverflow Discussion: [Calculate future/past date from a current date](https://stackoverflow.com/questions/6346119/datetime-add-days)