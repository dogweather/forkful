---
title:                "C#: 현재 날짜 받기."
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜

현재 날짜를 가져오는 것에 참여하는 이유는 매우 간단합니다. 우리 일상 생활에서는 시간과 날짜를 자주 다루게 됩니다. 예를 들어, 회의 일정 확인, 생일이나 기념일을 기록하는 등 여러 가지 일을 할 때 날짜는 중요한 역할을 합니다. 프로그래밍에서도 마찬가지입니다. 현재 날짜를 가져오는 것은 다양한 부서의 인력 및 일정 관리, 파일 저장, 로깅 등에 사용될 수 있기 때문입니다.

## 어떻게

현재 날짜를 가져오는 것은 C#에서 기본적으로 제공되는 DateTime 구조체를 사용하여 매우 쉽게 할 수 있습니다. 아래의 코드 예제를 통해 간단하게 확인해보세요.

```C#
// 현재 날짜를 가져오는 방법
DateTime currentDate = DateTime.Today;
```

위의 코드를 실행하면 현재 날짜를 Month/Day/Year 형식으로 출력할 수 있습니다.

```C#
// 출력 예시: 6/21/2021
```

또한, 원하는 형식의 날짜를 가져올 수도 있습니다. 예를 들어, 년도까지 포함된 전체 날짜를 가져오려면 아래와 같이 코드를 작성할 수 있습니다.

```C#
// 년도까지 포함된 전체 날짜를 가져오는 방법
DateTime currentDate = DateTime.Now;
```

위의 코드를 실행하면 현재 시간까지 포함된 날짜가 출력될 것입니다.

```C#
// 출력 예시: 6/21/2021 10:30:00 AM
```

## 딥 다이브

DateTime 구조체에는 날짜와 시간을 다양한 형식으로 가져올 수 있는 다양한 메서드와 속성이 있습니다. DateTime 구조체의 IsLeapYear 메서드를 사용하면 해당 년도가 윤년인지 아닌지를 확인할 수 있습니다. 또한, 날짜와 시간을 비교하거나 연산할 수도 있습니다. 이 외에도 다양한 기능들이 있으니 원하는 작업에 따라 적절한 메서드나 속성을 적용하여 사용해보세요.

## 참고 자료

- C# DateTime 구조체: https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime?view=net-5.0


"보기도"

- 윤년: https://ko.wikipedia.org/wiki/%EC%9C%A4%EB%85%84