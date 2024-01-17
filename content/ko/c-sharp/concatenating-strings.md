---
title:                "문자열 병합하기"
html_title:           "C#: 문자열 병합하기"
simple_title:         "문자열 병합하기"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇 & 왜? 
문자열 연결이란 무엇이며, 프로그래머들이 왜 그것을 하는지에 대해 두 덩어리로 나눠 봅시다.
문자열 연결은 단순히 문자열을 결합하는 것을 말합니다. 프로그래머들이 이 작업을 하는 이유는 기존의 문자열에 새로운 정보를 추가하기 위해서 입니다. 예를 들어, 사용자로부터 입력 받은 값과 일련의 문자열을 결합하여 완전히 새로운 문자열을 만들 수 있게 됩니다.

## 사용 방법:
아래의 예시 코드와 출력을 보면서 문자열 연결을 어떻게 할 수 있는지 알아보겠습니다.

```C#
// 변수선언 
string name = "Diana";
string favoriteColor = "Red";

// 문자열 연결
string sentence = "Hi, my name is " + name + ". My favorite color is " + favoriteColor + "!";

// 출력 
Console.WriteLine(sentence);
```

출력:

```
Hi, my name is Diana. My favorite color is Red!
```

## 더 깊게 파헤치기:
문자열 연결에 대해 더 알아봅시다!

- 역사적 배경: 이 기능은 원래 C 언어에서 사용되었으며, 다른 언어들에서도 자주 사용됩니다.
- 대안들: C#에서는 또 다른 방법으로 문자열 보간 (string interpolation)이 있습니다. 이 기능을 사용하면 더 간단한 방법으로 문자열을 결합할 수 있습니다.
- 구현 세부 사항: 문자열 연결은 두 개의 문자열을 결합할 때마다 새로운 메모리 공간을 할당합니다. 따라서 많은 문자열을 연결할 경우에는 성능이 저하될 수 있습니다.

## 관련 자료:
- [C# 문자열 문서](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
- [C# 문자열 보간](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)
- [C# 문자열 연산자](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/operators/concatenation-operator)