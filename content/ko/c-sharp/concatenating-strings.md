---
title:                "문자열 연결하기"
html_title:           "Arduino: 문자열 연결하기"
simple_title:         "문자열 연결하기"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?

문자열 연결(concatenation)은 여러 개의 문자열을 하나로 합치는 과정입니다. 이를 통해 프로그래머들은 정보를 효과적으로 표현하고 다룰 수 있게 됩니다.

## 사용방법:

다음은 C#에서 문자열을 연결하는 방법에 대한 코드 예제입니다.

```C#
string str1 = "Hello, ";
string str2 = "World!";
string result = str1 + str2;
Console.WriteLine(result);  // 출력: "Hello, World!"
```
또한, 이렇게 문자열을 연결하는 다른 방법도 있습니다:

```C#
string str3 = "Have ";
string str4 = "a good day!";
string result2 = String.Concat(str3, str4);
Console.WriteLine(result2);  // 출력: "Have a good day!"
```

## 심화 학습:

**역사**:
문자열 연결은 프로그래밍의 초기에부터 필수적인 기능 중 하나였습니다. C#에도 이 고전적인 기능이 포함될 수 있을 분명한 이유가 있었습니다.

**대체 수단**:
C#에는 `String.Concat` 나 `+` 연산자 외에도 `StringBuilder` 클래스를 이용할 수 있습니다. 스트링을 여러 번 연결하는 경우 `StringBuilder`가 더 효율적입니다.

```C#
StringBuilder builder = new StringBuilder();
builder.Append("Hello, ");
builder.Append("World!");

Console.WriteLine(builder.ToString()); // 출력: "Hello, World!"
```
**내부적인 작동 방식**:
C#의 문자열은 변경 불가능하며, 문자열을 연결할 때마다 실제로는 새 문자열이 생성됩니다. 그래서 많은 양의 문자열을 연결할 때는 비효율적이 될 수 있습니다. 이 문제를 해결하기 위해 `StringBuilder`를 사용할 수 있습니다.

## 참고 자료:

1. [Microsoft 문서: C# 문자열](https://docs.microsoft.com/ko-kr/dotnet/csharp/programming-guide/strings/)
2. [Stack Overflow: When to use StringBuilder in C#](https://stackoverflow.com/questions/407255/difference-between-stringbuilder-and-string-concatenation-stringbuilder-in-csh)