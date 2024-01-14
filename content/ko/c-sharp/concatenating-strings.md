---
title:                "C#: 문자열 연결하기"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## 왜

문자열을 연결하는 것에 대해 깊이 고민해보셨나요? C# 프로그래밍에서 문자열 연결은 매우 중요한 스킬입니다. 문자열 연결을 잘 활용하면 프로그램이 더욱 효율적이고 원하는 결과를 얻을 수 있습니다. 이번 포스트에서는 왜 우리가 문자열을 연결하는지에 대해 알아보도록 하겠습니다.

## 어떻게

내용은 다음과 같이 작성됩니다.

```C#
// 두 문자열을 연결하는 간단한 예제
string str1 = "안녕하세요, ";
string str2 = "저는 C# 프로그래머입니다.";
string result = str1 + str2;
Console.WriteLine(result);

// 결과: 안녕하세요, 저는 C# 프로그래머입니다.
```

문자열을 연결하는 방법에는 여러 가지가 있지만, 이번 포스트에서는 가장 간단하고도 자주 사용되는 `+` 연산자를 사용하는 방법을 설명하겠습니다.

문자열을 연결하기 위해서는 먼저 연결하고 싶은 문자열을 변수에 저장해야 합니다. 그 다음에는 `+` 연산자를 사용하여 두 문자열을 하나로 합쳐서 새로운 변수에 저장하면 됩니다. 연결한 후에는 `Console.WriteLine()` 함수를 사용하여 결과를 출력하면 됩니다.

## 깊이 들어가기

문자열을 연결해야 하는 이유는 다양합니다. 가장 간단한 이유는 문자열을 합쳐서 원하는 메시지를 만들어내는 것입니다. 예를 들어, 사용자로부터 이름과 나이를 입력받아서 "안녕하세요, 제 이름은 [이름]이고 제 나이는 [나이]살입니다."라는 메시지를 출력할 수 있습니다. 이때 문자열 연결을 사용하면 더욱 간편하게 원하는 메시지를 만들어낼 수 있습니다.

또 다른 이유는 파일 경로를 만들어내는 것입니다. 파일 경로는 일반적으로 문자열의 조각들을 합쳐서 만들어집니다. 예를 들어, `C:\Users\사용자이름\Documents\파일명.txt`와 같은 형식입니다. 이때 문자열 연결을 사용하면 각 조각들을 쉽게 합쳐서 파일 경로를 만들어낼 수 있습니다.

또한, 문자열을 연결할 때 주의할 점이 있습니다. 문자열을 연결할 때는 반드시 변수에 저장한 후에 연결해야 합니다. 만약 `Console.WriteLine(str1 + str2)`와 같은 형식으로 새로운 변수에 저장하지 않고 바로 출력하면 `str1`과 `str2`가 합쳐지지 않고 각각 출력됩니다. 따라서 문자열을 연결할 때는 주의해서 사용해야 합니다.

## 참고

- [C# 문자열 연결 방법](https://docs.microsoft.com/ko-kr/dotnet/csharp/how-to/concatenate-multiple-strings)
- [C# 문자열 연결에 대한 자세한 설명](https://www.tutorialspoint.com/csharp/csharp_string_concat.htm)
- [C# 문자열 합치기의 중요성](https://www.c-sharpcorner.com/article/why-string-concatenation-is-important-in-C-Sharp-programming/)