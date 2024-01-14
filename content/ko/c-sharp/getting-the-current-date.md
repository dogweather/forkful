---
title:    "C#: 현재 날짜 받기"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 왜?
현재 날짜를 얻는 것의 이유는 다양합니다. 예를 들어, 사용자의 생일을 확인하거나 현재 날짜에 따라 프로그램의 로직을 변경하는 등의 목적을 위해 사용할 수 있습니다.

## 방법
```C#
DateTime currentDate = DateTime.Today; // 오늘의 날짜를 가져옵니다.
Console.WriteLine(currentDate); // 현재 날짜를 출력합니다.
```

아래의 예시 코드는 현재 날짜를 포맷에 맞게 변환하는 방법을 보여줍니다.

```C#
DateTime currentDate = DateTime.Today;
string formattedDate = currentDate.ToString("yyyy-MM-dd");
Console.WriteLine(formattedDate); // 예시 출력: 2021-10-03
```

DateTime 클래스를 사용하면 시간과 날짜를 조작할 수 있는 다양한 메서드와 속성을 사용할 수 있습니다. 더 많은 정보는 MSDN 문서를 참고하세요.

## 깊게 들어가기
DateTime 클래스를 사용하여 현재 날짜를 얻는 방법을 살펴보았는데, 이 클래스는 시간대 정보까지 포함하는 시간과 날짜를 나타낼 수 있습니다. 또한 이 클래스는 날짜 및 시간 연산, 비교 등 다양한 기능을 제공합니다. 이를 통해 프로그램에서 날짜와 시간을 다루는데 유용하게 사용할 수 있습니다.

## 관련 링크
- [Microsoft Docs: DateTime 구조체 (C# 프로그래밍 가이드)](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime)
- [DateTime 클래스의 속성과 메서드 설명서 (MSDN)](https://docs.microsoft.com/ko-kr/dotnet/api/system.datetime?view=net-5.0#properties)
- [C# 프로그래밍을 위한 날짜 및 시간 처리 기술 (MSDN)](https://docs.microsoft.com/ko-kr/dotnet/standard/datetime/?view=net-5.0)