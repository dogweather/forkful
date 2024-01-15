---
title:                "문자열 대문자로 바꾸기"
html_title:           "C#: 문자열 대문자로 바꾸기"
simple_title:         "문자열 대문자로 바꾸기"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열의 첫 글자를 대문자로 만드는 것에 대해 생각해보셨나요? 이런 작업은 시스템에서 데이터를 효율적으로 정리하고 사람들이 이해하기 쉬운 방식으로 정보를 표현하는 데 도움이 됩니다.

## 방법

만약 문자열을 대문자로 바꾸고 싶다면, 다음과 같이 간단한 코드로 할 수 있습니다. 

```C#
string myString = "hello world"; 
string capitalizedString = myString.ToUpper(); 
Console.WriteLine(capitalizedString); 
```

이 코드의 출력은 "HELLO WORLD"가 됩니다. 문자열을 다룰 때 이렇게 대문자로 바꿔서 정리하는 것은 매우 유용합니다. 그렇지 않으면 많은 데이터 사이에서 원하는 것을 찾기 어려울 수 있습니다.

## 깊이 파고들기

자세히 들여다보면, 문자열을 대문자로 바꾸는 것은 C# 프로그래밍에서 상당히 간단한 작업입니다. 이 작업은 String 클래스의 ToUpper() 메소드를 사용해서 쉽게 할 수 있습니다. 또한 String 클래스에는 ToLower() 메소드도 있어서 문자열을 소문자로 바꾸는 것도 매우 간단합니다.

## 참고 자료

- [MSDN - String.ToUpper 메소드 (C#)](https://docs.microsoft.com/ko-kr/dotnet/api/system.string.toupper?view=netcore-3.1)
- [MSDN - String.ToLower 메소드 (C#)](https://docs.microsoft.com/ko-kr/dotnet/api/system.string.tolower?view=netcore-3.1)