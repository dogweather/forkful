---
title:                "C#: 두 날짜 비교하기"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 왜

날짜를 비교하는 이유는 여러 가지가 있겠지만, 가장 대표적인 이유는 어떤 이벤트나 작업을 수행하는 데에 있어서 특정한 날짜와 비교해서 조건을 만족시키기 위해서입니다. 예를 들어 사용자의 생일을 비교해서 생일 축하 메세지를 보여주는 등의 경우가 있을 수 있겠네요.

## 방법

C#에서 두 날짜를 비교하는 방법은 다양하지만, 가장 기본적인 방법은 "Compare" 함수를 사용하는 것입니다. 이 함수는 두 날짜를 비교해서 더 빠른 날짜라면 -1, 같다면 0, 더 늦은 날짜라면 1을 반환합니다. 아래는 예제 코드와 결과입니다.

```C#
DateTime date1 = new DateTime(2020, 5, 1);
DateTime date2 = new DateTime(2020, 5, 7);

int result = DateTime.Compare(date1, date2);

Console.WriteLine(result); // Output: -1
```

위의 예제에서는 date1이 date2보다 더 빠른 날짜이기 때문에 -1이 출력됩니다.

이외에도 두 날짜의 차이를 구하는 방법이나 특정 포맷으로 날짜를 출력하는 방법 등 다양한 방법이 존재합니다.

## 깊게 알아보기

날짜를 비교하는 방법은 보통 이전에 배운 조건문과 함께 사용하는 경우가 많습니다. 예를 들어, 어떤 이벤트를 특정 날짜와 비교해서 그 이전에는 "이벤트가 아직 시작하지 않았습니다"라는 메세지를 출력하고 이후에는 "이벤트가 이미 끝났습니다"라는 메세지를 출력하는 코드를 작성한다고 가정해봅시다. 이때 날짜를 비교하는 것은 매우 중요한 역할을 합니다.

하지만 날짜를 비교하는 것에는 유의할 점이 있습니다. 예를 들어 시간까지 동일한 두 날짜를 비교한다고 가정해봅시다. 이때는 "Compare" 함수 대신 "Date" 함수를 사용해야 시간을 제외한 날짜만을 비교할 수 있습니다. 또한 날짜 포맷에 따라서도 비교 결과가 달라질 수 있기 때문에 유의해야 합니다.

## 관련 글

* [DateTime.Compare 메서드 (Microsoft Docs)](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime.compare?view=netcore-3.1)
* [C#에서 날짜 비교하는 방법 (읍읍이의 공간)](https://leedeagu.tistory.com/entry/C-%EC%97%90%EC%84%9C-%EB%82%A0%EC%A7%9C-%EB%B9%84%EA%B5%90%ED%95%98%EB%8A%94-%EB%B0%A9%EB%B2%95)