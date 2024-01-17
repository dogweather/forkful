---
title:                "문자열의 길이 찾기"
html_title:           "C#: 문자열의 길이 찾기"
simple_title:         "문자열의 길이 찾기"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
문자열의 길이를 찾는 것은 프로그래머들이 코딩을 할 때 매우 중요한 기술입니다. 문자열의 길이를 찾는 것은 특정 데이터나 변수가 얼마나 긴지를 알아내는 것을 도와줍니다. 이것은 데이터를 처리하고 조작하는 프로그래밍에서 핵심 기능 중 하나입니다.

## 방법:
```
C# string str = "Hello, world!";
int strLength = str.Length;
Console.WriteLine(strLength);
```
위 코드는 문자열의 길이를 찾는 가장 간단하고 기본적인 방법입니다. 변수 뒤에 ```.Length```를 붙여주면 해당 변수의 길이를 반환하는 기능을 가지고 있는 것을 확인할 수 있습니다. 위 코드를 실행하면 ```13```이라는 결과가 나옵니다.

## 더 깊이 알아보기:
1. 초기에는 문자열의 길이를 찾는 것은 매우 간단한 작업이었습니다. 하지만 컴퓨터의 발전으로 인해 문자열의 길이뿐만 아니라 문자열 내부에 있는 각 문자의 바이트 수까지도 알아내는 것이 가능해졌습니다.
2. 다른 언어에서도 문자열의 길이를 찾는 방법은 비슷합니다. 하지만 그 언어마다 조금씩 차이가 있을 수 있으니 주의해야 합니다.
3. 문자열의 길이를 찾는 다른 방법으로는 ```foreach```문을 사용하는 것이 있습니다. 이 방법은 문자열의 각 문자를 순차적으로 접근하는데 유용합니다.

## 관련 자료:
- [Microsoft Docs - String.Length Property](https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=net-5.0)
- [C# Program Structure](https://www.telerik.com/blogs/c-program-structure)
- [GeeksforGeeks - Finding Length of String in C#](https://www.geeksforgeeks.org/finding-length-of-string-in-c-sharp/)