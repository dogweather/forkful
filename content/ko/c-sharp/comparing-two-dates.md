---
title:                "C#: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜
일반적으로 날짜를 비교하는 것은 프로그래밍에서 자주 사용되는 작업입니다. 두 날짜를 비교하면, 날짜가 이전인지, 이후인지, 또는 같은지를 판단할 수 있습니다. 이는 일정 기간 이내에 발생한 이벤트를 추적하거나, 두 날짜 사이의 차이를 계산하는 데 유용합니다.

## 방법
날짜 비교는 C#에서 매우 간단한 작업입니다. 비교하려는 두 날짜를 DateTime 형식으로 선언하고, ">" 또는 "<" 연산자를 사용하여 비교하면 됩니다. 예를 들어, 다음과 같습니다.

```C#
DateTime date1 = new DateTime(2021, 4, 15);
DateTime date2 = new DateTime(2021, 4, 16);

if (date1 > date2)
{
    Console.WriteLine("date1 is later than date2");
}
else if (date1 < date2)
{
    Console.WriteLine("date1 is earlier than date2");
}
else
{
    Console.WriteLine("date1 and date2 are the same date");
}

// Output: date1 is earlier than date2
```

여기서 주의할 점은 DateTime 변수를 선언할 때, year, month, day를 인자로 넘겨줘야 한다는 것입니다. 그렇지 않으면 에러가 발생할 수 있습니다.

## 깊이 들어가기
날짜를 비교하는 더 많은 방법이 있지만, 모두 위에서 언급한 방법과 비슷합니다. 연, 월, 일, 시, 분, 초, 밀리초 등 더 세부적인 부분으로도 비교할 수 있습니다. 또한, 두 날짜 사이의 차이를 계산하는 방법도 있습니다. 이는 TimeSpan 클래스를 사용하여 가능합니다.

간단한 예시를 보겠습니다.

```C#
DateTime date1 = new DateTime(2021, 4, 15, 15, 30, 0);
DateTime date2 = new DateTime(2021, 4, 15, 10, 30, 0);

TimeSpan difference = date1 - date2;
Console.WriteLine("The difference in hours is: " + difference.TotalHours);

// Output: The difference in hours is: 5
```

더 많은 정보를 알고 싶다면, MSDN 레퍼런스나 다른 온라인 자료를 참고하시면 됩니다.

## 더 읽어보기
* [DateTime 클래스에 대한 MSDN 레퍼런스](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
* [Comparing Dates and Times in C#](https://www.c-sharpcorner.com/UploadFile/mahesh/comparing-dates-and-times-in-C-Sharp/)