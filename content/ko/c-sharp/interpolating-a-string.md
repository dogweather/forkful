---
title:                "문자열 보간하기"
html_title:           "Java: 문자열 보간하기"
simple_title:         "문자열 보간하기"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 왜하고 뭐해?

스트링 인터폴레이션은 문자열 내 값들을 표현하기 위한 방법으로, 이는 코드의 가독성을 향상하고 코딩 실수를 줄이는 것에 도움을 줍니다.

## 어떻게?

스트링 인터폴레이션을 사용하면 문자열 안에 직접 변수나 식을 삽입할 수 있습니다. C#에서 이와같이 하면:

```C#
int age = 10;
string name = "Jin";
Console.WriteLine($"Hello {name}, you are {age} years old.");
```

출력 결과는 이렇습니다:

```Output
Hello Jin, you are 10 years old.
```

## 깊게 알아봅시다

스트링 인터폴레이션은 C# 6.0에서 처음 도입되었습니다. 이전 버전에는 `String.Format` 메소드나 차례대로 더하는 연산자 (`+`)를 이용해 문자열 내에 값을 표현했었습니다.

```C#
int age = 10;
string name = "Jin";
string message = String.Format("Hello {0}, you are {1} years old.", name, age);
Console.WriteLine(message);
// or
Console.WriteLine("Hello " + name + ", you are " + age + " years old.");
```

하지만 위의 방법들은 가독성이 떨어지며, 잘못된 인덱스나 더하기 연산자의 과다 사용으로 인한 오류를 일으킬 수 있습니다. 그런 점에서 C# 6.0 이후로 도입된 스트링 인터폴레이션은 이와같은 문제를 해결해, 개발자들에게 많은 도움을 제공하고 있습니다.

## 참고 자료

스트링 인터폴레이션에 대해 더 알아보려면 Microsoft가 제공하는 다음 자료를 참조할 수 있습니다:

1. [String interpolation (C# reference)](https://docs.microsoft.com/ko-kr/dotnet/csharp/language-reference/tokens/interpolated)
2. [String.Format 메소드](https://docs.microsoft.com/ko-kr/dotnet/api/system.string.format?view=net-5.0)