---
title:                "현재 날짜 가져오기"
html_title:           "C#: 현재 날짜 가져오기"
simple_title:         "현재 날짜 가져오기"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 현재 날짜 가져오기: C# 프로그래머를 위한 가이드

## What & Why?
현재 날짜를 가져오는 것은 프로그래머에게 중요한 작업 중 하나입니다. 현재 날짜를 가져오면 프로그램 내에서 다양한 용도로 사용할 수 있습니다. 예를 들어, 이를 활용하여 유효성 검사를 수행하거나 파일 이름에 날짜를 포함할 수 있습니다. 그리고 현재 날짜를 가져오는 것은 프로그램을 보다 정확하고 유용하게 만들어줍니다.

## How to:
```C#
DateTime currentDate = DateTime.Now;
Console.WriteLine(currentDate);
// 출력 결과: 2020-07-30 10:00:00 AM
```

위의 예시에서는 ```DateTime``` 클래스의 ```Now``` 메소드를 사용하여 현재 날짜와 시간을 가져오고, ```Console``` 클래스의 ```WriteLine``` 메소드를 통해 콘솔에 출력하는 방법을 보여줍니다. 또 다른 방법으로는, ```ToString``` 메소드를 사용하여 원하는 형식에 맞춰 날짜와 시간을 출력할 수도 있습니다.

```C#
DateTime currentDate = DateTime.Now;
string formattedDate = currentDate.ToString("MM-dd-yyyy");
Console.WriteLine(formattedDate);
// 출력 결과: 07-30-2020
```

## Deep Dive:
현재 날짜를 가져오는 ```DateTime``` 클래스는 .NET 프레임워크의 일부로 제공됩니다. 이 클래스는 날짜와 시간을 단일 값으로 저장하고 조작할 수 있는 매우 유용한 기능을 제공합니다. 또한, 시스템 설정에 따라 날짜와 시간의 형식이 달라질 수 있으므로 설정에 주의해야 합니다.

다른 프로그래밍 언어에서도 현재 날짜를 가져오는 다양한 방법이 있지만, C#의 ```DateTime``` 클래스는 날짜와 시간을 일관된 형식으로 조작할 수 있도록 도와줍니다. 또한, 특정 시간대의 날짜와 시간을 가져오는 기능도 있어 다국적 어플리케이션 개발에 유용합니다.

## See Also:
- [Microsoft Docs: DateTime 구조체](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime?view=netcore-3.1)
- [Microsoft School: 날짜와 시간 다루기](https://microsoft.github.io/MicrosoftSchool/korea/datetime.html)
- [YouTube: C# DateTime 사용하기](https://www.youtube.com/watch?v=cZ-4LMz_WF0)