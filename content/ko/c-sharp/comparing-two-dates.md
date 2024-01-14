---
title:    "C#: 두 날짜 비교하기"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

날짜를 비교하는 것에 관심이 있을 수도 있습니다. 이것은 가장 흔한 프로그래밍 작업 중 하나이며, 두 날짜 간의 차이를 계산하거나, 특정 날짜보다 전일인지 후일인지 확인할 때 유용합니다.

## 하우 투

```C#
DateTime today = DateTime.Today;
DateTime tomorrow = today.AddDays(1);

Console.WriteLine("오늘 날짜는 " + today);
Console.WriteLine("내일 날짜는 " + tomorrow);
Console.WriteLine("오늘 날짜보다 내일 날짜는 " + tomorrow.Subtract(today).Days + " 일 후입니다.");

/*
* 출력:
* 오늘 날짜는 11/27/2020 00:00:00
* 내일 날짜는 11/28/2020 00:00:00
* 오늘 날짜보다 내일 날짜는 1 일 후입니다.
*/
```

위 예시 코드에서는 C#의 `DateTime` 클래스를 사용하여 오늘 날짜를 얻고, `AddDays()` 메서드를 사용하여 내일 날짜를 계산합니다. 또한 `Subtract()` 메서드를 사용하여 오늘 날짜와 내일 날짜 간의 차이를 계산합니다.

## 딥 다이브

C#에서는 `DateTime` 클래스를 사용하여 다양한 날짜 비교 작업을 수행할 수 있습니다. 예를 들어, `Equals()` 메서드를 사용하여 두 날짜가 동일한 날짜인지 확인할 수 있습니다. 또한 `Compare()` 메서드를 사용하여 두 날짜를 비교하여 더 이전인지, 같은 날짜인지, 더 이후인지를 알 수 있습니다. `ToString()` 메서드를 사용하면 날짜를 원하는 형식으로 출력할 수도 있습니다.

## 참고

- [Microsoft docs - Comparing Dates and Times in C#](https://docs.microsoft.com/en-us/dotnet/standard/datetime/comparing-dates)
- [GeeksforGeeks - Date Comparisons in C#](https://www.geeksforgeeks.org/date-comparisons-in-c-sharp/)
- [C# Corner - Comparing DateTime Objects in C#](https://www.c-sharpcorner.com/article/comparing-datetime-objects-in-c-sharp/)