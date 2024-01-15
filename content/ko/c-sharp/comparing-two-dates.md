---
title:                "두 날짜 비교"
html_title:           "C#: 두 날짜 비교"
simple_title:         "두 날짜 비교"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜?

날짜 비교는 두 가지 날짜가 얼마나 다른지를 확인하거나 두 날짜 사이의 경과 시간을 계산하는데 유용합니다.

## 하는 방법

```C#
var date1 = new DateTime(2021, 7, 15);
var date2 = new DateTime(2021, 7, 20);

// 두 날짜의 차이를 구하는 방법
TimeSpan difference = date2 - date1;
Console.WriteLine(difference.Days); // 출력: 5

// 날짜가 미래인지 과거인지 확인하는 방법
if (date1 > date2)
{
    Console.WriteLine("date1은 date2보다 미래입니다."); 
}
else if (date1 < date2)
{
    Console.WriteLine("date1은 date2보다 과거입니다."); 
}
else
{
    Console.WriteLine("date1과 date2는 같은 날짜입니다."); 
}
```

출력: date1은 date2보다 과거입니다.

## 깊이 파고들기

날짜 비교에는 여러가지 방법이 있습니다. IsSameDay() 함수를 만들어서 두 날짜의 년, 월, 일이 모두 일치하는지 확인하는 방법도 있고, Compare() 함수를 사용하여 두 날짜를 비교하는 방법도 있습니다. 또한 DateTime 구조체의 메서드를 사용하여 특정 날짜의 요일이나 월의 일 수를 가져올 수도 있습니다.

## 참고

- [C# DateTime 클래스 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime?view=net-5.0)
- [C# TimeSpan 구조체 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.timespan?view=net-5.0)
- [C# 비교 연산자](https://docs.microsoft.com/ko-kr/dotnet/csharp/language-reference/operators/comparison-operators)