---
title:                "C#: 미래나 과거의 날짜 계산하기"
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

미래나 과거의 날짜를 계산하는 것에 참여하는 이유는 다양합니다. 예를 들어, 다가오는 이벤트나 발표일을 계획하고 조직하기 위해서일 수 있습니다. 또는 기간과 기한을 추적하거나 기간을 계산하는 데 도움을 줄 수 있습니다.

## 어떻게

```c#
// 현재 날짜 
DateTime today = DateTime.Today;

// 미래의 날짜 계산 (예: 2주 후) 
DateTime futureDate = today.AddDays(14);

// 과거의 날짜 계산 (예: 3달 전) 
DateTime pastDate = today.AddMonths(-3);

// 출력 
Console.WriteLine("오늘: " + today.ToShortDateString());
Console.WriteLine("미래의 날짜: " + futureDate.ToShortDateString());
Console.WriteLine("과거의 날짜: " + pastDate.ToShortDateString());

// 출력 결과:
// 오늘: 3/22/2021
// 미래의 날짜: 4/5/2021
// 과거의 날짜: 12/22/2020
```

## 더 들어가기

날짜를 보다 정확하게 계산하려면 DateTime 구조체와 함께 사용할 수 있는 다양한 속성과 메서드를 살펴보는 것이 중요합니다. 이를 통해 날짜와 시간 간의 차이를 계산하고, 특정 요일이나 특정 날짜 인스턴스를 찾는 등 다양한 작업을 수행할 수 있습니다.

## 더 알아보기

- [DateTime 구조체 및 관련 스니펫 코드](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime?view=net-5.0)
- [DateTime 구조체 및 TimeSpan 구조체 비교](https://www.titanwolf.org/Network/q/bb96bbce-0949-427e-a6e0-57ffdc371674/y)
- [날짜 및 시간 맞춤형 출력 포맷의 다양한 옵션](https://docs.microsoft.com/ko-kr/dotnet/standard/base-types/custom-date-and-time-format-strings)