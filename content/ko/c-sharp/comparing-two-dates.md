---
title:    "C#: 두 날짜 비교하기"
keywords: ["C#"]
---

{{< edit_this_page >}}

## 왜

날짜를 비교하는 것의 장점은 여러 가지가 있습니다. 예를 들어, 날짜를 비교함으로써 특정 이벤트가 언제 발생해야 하는지 결정할 수 있고, 만료일을 확인하고 만료 예정인 항목을 식별하는 등의 유용한 작업을 할 수 있습니다.

## 방법

C#에서 두 날짜를 비교하는 것은 간단합니다. 아래의 코드 예시를 참고하세요.

```C#
DateTime firstDate = DateTime.Parse("2021-04-03");
DateTime secondDate = DateTime.Parse("2021-04-05");

int result = firstDate.CompareTo(secondDate);
// result의 값이 0보다 작으면 firstDate가 secondDate보다 이전 날짜이고, 0이면 두 날짜가 같으며, 0보다 크면 firstDate가 secondDate보다 이후 날짜입니다. 
```

위 코드에서는 먼저 `DateTime` 형식의 변수를 선언하고 `Parse` 메소드를 사용해 문자열로 된 날짜를 `DateTime` 형식으로 변환해줍니다. 그리고 `CompareTo` 메소드를 사용해 두 날짜를 비교하고 그 결과를 `int` 형식의 변수에 할당합니다. 이렇게 함으로써 두 날짜의 순서를 판단할 수 있게 됩니다.

## 딥 다이브

두 날짜를 비교하는 방법은 C#의 `DateTime` 형식과 관련된 많은 속성과 메소드를 알아야 합니다. 예를 들어, `DateTime` 형식에는 `Date`, `Month`, `Year` 등의 속성이 있으며 `AddDays`, `AddMonths`, `AddYears` 등의 메소드를 사용해 날짜를 변경할 수 있습니다. 이러한 속성과 메소드를 적절하게 활용하면 복잡한 날짜 비교도 쉽게 할 수 있습니다.

## 관련 정보

- [Microsoft Docs - DateTime.Compare 메소드](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime.compare?view=net-5.0)
- [Microsoft Docs - DateTime.CompareTo 메소드](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime.compareto?view=net-5.0)
- [C# 날짜와 시간 다루기](https://www.c-sharpcorner.com/UploadFile/mahesh/datetime-in-C-Sharp/)