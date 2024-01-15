---
title:                "미래 또는 과거의 날짜 계산"
html_title:           "C#: 미래 또는 과거의 날짜 계산"
simple_title:         "미래 또는 과거의 날짜 계산"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 왜

일일이 날짜를 계산하는 것은 번거로운 일일 수 있습니다. 하지만 현재 날짜에서 일정한 기간을 더하거나 빼는 등의 계산을 하려면 이 방법이 가장 간단하고 효율적인 방법입니다.

## 시작하기

먼저, 다음과 같이 `DateTime` 형식의 변수를 선언해줍니다.

```C#
DateTime today = DateTime.Today;
```

그리고 `AddDays()` 메소드를 사용하여 미래나 과거의 날짜를 계산할 수 있습니다.

```C#
// 3일 후의 날짜 계산
DateTime futureDate = today.AddDays(3);

// 1주 전의 날짜 계산
DateTime pastDate = today.AddDays(-7);
```

계산한 날짜를 `Console.WriteLine()` 메소드를 사용해 출력해보면 다음과 같이 나타납니다.

```
2021-09-26
```

위와 같이 `DateTime` 변수에 저장된 날짜는 연, 월, 일 순서대로 출력됩니다.

## 깊게 파보기

`DateTime` 변수의 기본 값을 설정하지 않고 선언하면 현재 시간을 자동으로 가져옵니다. 만약 원하는 시간을 함께 계산하고 싶다면 `DateTime` 객체를 생성할 때 시, 분, 초 값을 함께 설정할 수 있습니다.

```C#
DateTime now = new DateTime(2021, 9, 26, 17, 30, 0);
```

위와 같이 세 번째 파라미터에 원하는 날짜의 시간 값을 넣어주면 해당 시간을 기준으로 계산할 수 있습니다.

## 참고 문서

- [C# DateTime 구조 (Microsoft Docs)](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime?view=net-5.0)