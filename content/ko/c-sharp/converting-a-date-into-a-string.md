---
title:                "C#: 날짜를 문자열로 변환하기"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜
한국어 버전

일정을 문자열로 변환하는 일에 참여하는 이유는 무엇일까요? 이는 프로그래밍에서 매우 일반적인 과제 중 하나입니다. 일정을 문자열로 표현하면 다른 기능과 조합하여 강력한 프로그램을 만들 수 있습니다. 이렇게 표현하는 방법은 매우 유연하며 문제를 해결하는 데 매우 유용합니다.

## 이와 같이
코딩 예제와 "```C# ... ```" 코드블록 내에서의 샘플 출력입니다.

```C#
DateTime now = DateTime.Now;
string dateToString = now.ToString();
Console.WriteLine(dateToString);
// 출력: 2021-05-04 10:30:00
```

매우 쉽죠? 날짜를 문자열로 변환하기 위해 사용할 수 있는 여러 가지 방법이 있습니다. 이 예제에서는 기본 `ToString` 메서드를 사용했지만, 서식 지정 문자열을 사용하거나 날짜 형식을 직접 지정하는 방법도 있습니다. 아래 링크에서 더 많은 정보를 확인할 수 있습니다.

## 깊은 곳으로
날짜를 문자열로 변환하는 데는 많은 내부적인 처리 과정이 있습니다. 날짜 형식, 로컬 또는 UTC 시간, 다국어 지원 등 여러 가지 요소를 고려해야 합니다. 또한 분명히 `ToString` 메서드는 매우 강력하고 유연한 기능을 제공합니다. 따라서 이를 효과적으로 이용하면 더 많은 기능을 가진 프로그램을 만들 수 있습니다.

## 이와 관련하여
다른 관련 정보를 확인해보세요. 아래 링크에서 더 많은 내용을 확인할 수 있습니다.

1. [DateTime.ToString 메서드 - C# 및 .NET for Beginners](https://csharp.today/csharp-datetime-tostring-method/)
2. [.NET 날짜 표기법 및 날짜 형식 문자열 참조 - Microsoft Docs](https://docs.microsoft.com/ko-kr/dotnet/standard/base-types/custom-date-and-time-format-strings)
3. [Why is converting a DateTime to a string such a common task in C#? - Stack Overflow](https://stackoverflow.com/questions/10705905/why-is-converting-a-datetime-to-a-string-such-a-common-task-in-c)