---
title:                "C#: 날짜를 문자열로 변환하기"
simple_title:         "날짜를 문자열로 변환하기"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 왜

날짜를 문자열로 변환하는 일이 필요한 이유는 다양합니다. 예를 들어, 사용자에게 날짜 정보를 표시하거나 데이터베이스에 저장하기 위해 날짜를 문자열로 변환할 수 있습니다. C#에서는 날짜를 문자열로 변환하기 위한 다양한 방법을 제공하기 때문에, 우리는 자유롭게 원하는 방식으로 날짜를 문자열로 변환할 수 있습니다.

## 방법

C#에서 날짜를 문자열로 변환하는 가장 간단한 방법은 ```ToString()``` 함수를 사용하는 것입니다. 아래의 예제 코드를 참고해 주세요.

```C#
DateTime today = DateTime.Today;
string stringDate = today.ToString();
Console.WriteLine(stringDate);

// 출력 결과:
// 11/18/2021 12:00:00 AM
```

우리가 기대한 대로, 날짜가 문자열로 변환되어 출력되었습니다. 날짜를 원하는 형식으로 표시하기 위해서는 ```ToString()``` 함수의 매개변수에 형식을 지정해 주어야 합니다. 아래의 예제 코드를 참고해 주세요.

```C#
DateTime today = DateTime.Today;
string stringDate = today.ToString("MM/dd/yyyy");
Console.WriteLine(stringDate);

// 출력 결과:
// 11/18/2021
```

또 다른 방법으로는 ```String.Format()``` 함수를 사용하는 것입니다. 이 함수를 사용하면, 문자열 안에서 직접 형식을 지정할 수 있습니다. 아래의 예제 코드를 참고해 주세요.

```C#
DateTime today = DateTime.Today;
string stringDate = String.Format("오늘은 {0:MM월 dd일 yyyy년} 입니다.", today);
Console.WriteLine(stringDate);

// 출력 결과:
// 오늘은 11월 18일 2021년 입니다.
```

## 딥 다이브

C#에서는 날짜를 문자열로 변환하기 위해 다양한 형식을 제공합니다. 그 중 가장 일반적으로 사용되는 형식은 "MM/dd/yyyy" 형식입니다. 하지만, 우리가 원하는 대로 날짜를 형식화하기 위해서는 사용 가능한 형식을 이해해야 합니다. 아래의 링크를 통해 날짜 형식에 대해 더 자세히 알아보세요.

## 참고 자료

- [C# 날짜 형식 지정 방법](https://docs.microsoft.com/ko-kr/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [코딩 시작하기: 날짜와 시간 다루기](https://docs.microsoft.com/ko-kr/dotnet/csharp/programming-guide/dates-and-times/)