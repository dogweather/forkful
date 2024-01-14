---
title:                "C#: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜
현재 날짜를 얻는 프로그래밍을 하는 이유는 다양합니다. 예를 들어, 현재 날짜를 데이터베이스에 저장하거나 특정 이벤트를 기록할 때 유용합니다. 또는 현재 날짜를 기준으로 조건문을 작성하여 특정 작업을 수행하는 것도 가능합니다.

## 하우 투
C#에서는 `DateTime` 클래스를 사용하여 현재 날짜를 얻을 수 있습니다. 먼저, 네임스페이스를 추가해야 합니다.
```
using System;
```
그리고 다음과 같이 `DateTime.Now` 메서드를 호출합니다.
```
DateTime currentDate = DateTime.Now;
Console.WriteLine(currentDate);
```
위의 코드를 실행하면 현재 날짜와 시간이 출력됩니다. 또는 원하는 형식으로 표시할 수도 있습니다.
```
Console.WriteLine(currentDate.ToString("yyyy년 MM월 dd일"));
```
출력 결과는 "2021년 06월 30일"과 같이 나타납니다.

## 딥 다이브
`DateTime` 클래스에는 다양한 메서드와 속성이 있어서 현재 날짜와 시간을 다양하게 조작할 수 있습니다. 예를 들어, `AddDays()` 메서드를 사용하여 현재 날짜에 일수를 더하거나 빼는 것이 가능합니다. 또는 `DayOfWeek` 속성을 이용하여 현재 날짜의 요일을 확인할 수도 있습니다.

또한, `DateTime` 클래스 외에도 `DateTimeOffset` 클래스를 사용하여 현재 날짜와 시간을 지역별로 나타낼 수도 있습니다. 또는 `TimeZoneInfo` 클래스를 사용하여 다양한 시간대의 정보를 확인할 수 있습니다.

## 참고 자료
- [Microsoft Docs - DateTime 구조체](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime?view=net-5.0)
- [Microsoft Docs - DateTimeOffset 구조체](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetimeoffset?view=net-5.0)
- [Microsoft Docs - TimeZoneInfo 클래스](https://docs.microsoft.com/ko-kr/dotnet/api/system.timezoneinfo?view=net-5.0)