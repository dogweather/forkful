---
title:                "현재 날짜 받아오기"
html_title:           "C#: 현재 날짜 받아오기"
simple_title:         "현재 날짜 받아오기"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜?

오늘 날짜를 얻는 것의 경우, 개발자에게 가장 일상적이고 기본적인 작업 중 하나입니다. 코드가 현재 날짜를 알고 있다면, 다양한 애플리케이션에서 매우 유용하게 사용할 수 있습니다.

## 어떻게?

```C#
DateTime now = DateTime.Now;
Console.WriteLine("Today is: " + now.ToString("yyyy-MM-dd"));
```

위의 코드를 실행하면 현재 날짜가 출력됩니다. `DateTime.Now` 메소드는 현재 날짜와 시간을 나타내는 `DateTime` 객체를 반환합니다. 이 객체를 사용하여 해당하는 날짜 형식으로 출력할 수 있습니다.

```C#
DateTime now = DateTime.Now;
Console.WriteLine("Today is: " + now.ToString("dddd"));
```

위의 코드를 실행하면 현재 요일명이 출력됩니다. `dddd` 형식 지정자를 사용하여 날짜를 원하는 형식으로 가져올 수 있습니다. 또한 `DateTime` 객체에서 원하는 날짜 정보를 `Year`, `Month`, `Day`, `Hour` 등의 속성을 사용하여 가져올 수도 있습니다.

## 깊게 들어가보기

`.Now` 메소드는 시스템의 현재 시간을 기준으로 한 `DateTime` 개체를 반환합니다. 이는 로컬 시간이 아닌 시스템의 시간을 기준으로 한 것입니다. 따라서 서로 다른 시간대에 있는 두 컴퓨터로부터 동일한 코드를 실행하면, 결과는 다를 수 있습니다.

또한, `DateTime.Now` 메소드는 컴퓨터의 클럭에서 가져오는 시스템 시간을 기준으로 합니다. 따라서 사용자의 컴퓨터 시간이 올바르게 설정되어 있지 않을 경우, 결과 또한 잘못된 날짜와 시간을 반환할 수 있습니다.

## 참고

[DateTime 구조체 - MSDN](https://msdn.microsoft.com/ko-kr/library/system.datetime(v=vs.110).aspx)

[C# 날짜와 시간 포매팅 및 분석 - Microsoft Docs](https://docs.microsoft.com/ko-kr/dotnet/standard/base-types/custom-date-and-time-format-strings)

[KST 지원을 위한 C# 날짜 및 시간 조정하기 - C# 코너](https://www.codeproject.com/Articles/12396/Clock-A-DateTime-based-Adventure-Game#KoreaStandardTime)