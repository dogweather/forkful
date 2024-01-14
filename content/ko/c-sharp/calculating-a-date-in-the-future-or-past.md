---
title:                "C#: 미래나 과거의 날짜 계산하기"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 왜

우리는 일상 생활에서 여러 가지 이유로 지난 날짜나 미래의 날짜를 계산하는 경우가 있습니다. 예를 들어, 생일이나 결혼 기념일을 계산하거나, 여행 계획을 세울 때 등등 말이죠. 이러한 상황에서 우리는 프로그래밍을 통해 특정한 날짜를 자동으로 계산할 수 있습니다. 이러한 기능은 우리의 일상 생활을 더 쉽고 편리하게 만들어 줍니다.

# 방법

날짜를 계산하는 방법은 매우 간단합니다. 우선, 기준이 되는 날짜를 입력해야 합니다. C#에서는 DateTime 객체를 사용해서 날짜를 나타냅니다. 예를 들어, 2021년 5월 20일을 표현하려면 다음과 같이 입력할 수 있습니다.

```C#
DateTime now = new DateTime(2021, 5, 20);
```

이제 우리가 원하는 만큼 날짜를 더하거나 빼서 계산할 수 있습니다. 날짜를 더하거나 빼는 방법은 AddDays(), AddMonths(), AddYears() 메서드를 사용하는 것입니다. 예를 들어, 5일 후의 날짜를 계산하고 싶다면 다음과 같이 입력합니다.

```C#
DateTime futureDate = now.AddDays(5);
```

이렇게 하면 now에 저장된 날짜에서 5일을 더한 날짜가 futureDate 변수에 저장됩니다. 또한, 날짜 차이를 계산하는 방법으로는 DateDiff() 메서드가 있습니다. 이 메서드를 사용하면 두 날짜 사이의 일수를 계산할 수 있습니다.

# 깊게 들어가기

C#에서는 DateTime 객체를 사용하여 날짜를 다루기 때문에 많은 유용한 메서드를 제공합니다. Add(), Substract(), Compare() 등 다양한 메서드를 통해 날짜를 연산하고 비교할 수 있습니다. DateTime 객체를 다루는 방법을 잘 익혀두면 미래나 과거의 날짜를 계산하는 데 매우 유용합니다.

# 이어보기

- [C#에서 날짜 계산하기](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime)
- [DateTime 구조체](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime?view=net-5.0)
- [Date and Time Format Strings](https://docs.microsoft.com/ko-kr/dotnet/standard/base-types/custom-date-and-time-format-strings?view=net-5.0)