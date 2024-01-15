---
title:                "문자열 연결"
html_title:           "C#: 문자열 연결"
simple_title:         "문자열 연결"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

문자열을 연결하는 것의 중요성은 우리가 자주 수행하는 작업 중 하나입니다. 예를 들어, 이름과 성을 연결하여 전체 이름을 만들거나 여러 정보를 하나의 문자열로 합치는 경우가 있습니다. 이를 효과적으로 수행하기 위해서는 문자열을 연결하는 방법을 알고 있어야 합니다.

## 방법

문자열을 연결하는 가장 간단한 방법은 "+" 연산자를 사용하는 것입니다. 아래는 두 개의 문자열을 연결하는 예시입니다.

```C#
string firstName = "John";
string lastName = "Doe";
string fullName = firstName + lastName;

Console.WriteLine(fullName);
```

출력은 `JohnDoe`가 될 것입니다.

또한 여러 개의 문자열을 한 번에 연결할 수도 있습니다. 이 경우에는 `string.Join()` 메서드를 사용하면 됩니다. 아래는 세 개의 문자열을 연결하는 예시입니다.

```C#
string address = "123 Main St.";
string city = "Seoul";
string country = "South Korea";
string fullAddress = string.Join(", ", address, city, country);

Console.WriteLine(fullAddress);
```

출력은 `123 Main St., Seoul, South Korea`가 될 것입니다.

## 더 깊게 알아보기

문자열을 연결하는 방법으로는 위의 예시 외에도 다양한 방법이 있습니다. 예를 들어, `StringBuilder` 클래스를 사용하는 것이 있습니다. 이는 여러 문자열을 하나의 문자열로 합칠 때 성능을 향상시킬 수 있는 방법입니다. 또한 `Format()` 메서드를 사용하여 문자열 내에 변수를 삽입할 수도 있습니다. 아래는 두 개의 변수를 사용하여 문자열을 연결하는 예시입니다.

```C#
string language = "C#";
string version = "8.0";
string intro = string.Format("This is {0} version {1}", language, version);

Console.WriteLine(intro);
```

출력은 `This is C# version 8.0`이 될 것입니다.

## 더 알아보기

- [String Concatenation in C#](https://www.geeksforgeeks.org/string-concatenation-in-c-sharp/)
- [Using String.Format in C#](https://www.c-sharpcorner.com/article/using-string-format-in-c-sharp/)
- [Understanding the StringBuilder Class in C#](https://www.c-sharpcorner.com/article/understanding-the-stringbuilder-class-in-c-sharp/)

## 관련 링크

- [C# Tutorial for Beginners](https://www.tutorialspoint.com/csharp/index.htm)
- [.NET Documentation](https://docs.microsoft.com/en-us/dotnet/)
- [C# String Concepts](https://www.tutorialsteacher.com/csharp/csharp-string)