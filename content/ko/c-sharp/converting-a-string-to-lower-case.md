---
title:                "C#: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜
문자열을 소문자로 변환하는 것에 참여하는 이유는 다양합니다. 예를 들어, 검색 엔진에서 대소문자를 구분하지 않는 경우, 소문자로 변환하여 일관된 결과를 얻기 위해 사용할 수 있습니다.

## 하는 방법
문자열의 소문자 변환은 C#의 ToLower() 메소드를 사용하여 쉽게 할 수 있습니다. 아래는 ToLower() 메소드를 사용한 예제 코드와 그 결과입니다.
```C#
string str = "TESTING";
Console.WriteLine(str.ToLower());
// Output: testing
```

문자열의 모든 문자를 소문자로 변환하는 것이 아니라, 특정 문자열만 소문자로 변환하고 싶은 경우에는 ToLower() 메소드 대신에 String.ToLowerInvariant() 메소드를 사용할 수도 있습니다.

```C#
string str = "Testing";
string str2 = "tESTING";
Console.WriteLine(str.ToLowerInvariant());
Console.WriteLine(str2.ToLowerInvariant());
// Output: testing
// testing
```
## 더 깊게 들어가기
문자열의 소문자 변환의 경우, .NET Framework의 문자 변환 규칙을 따라 소문자로 변환됩니다. 하지만 이는 특수한 사례가 있을 수 있습니다. 예를 들어, 터키어에서 대소문자 변환 시 변경되는 문자열이 있습니다.

## 더 알아보기
- 문자열 변환에 대한 공식 문서: https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=netframework-4.8
- 터키어에서 대소문자 변환이 다른 언어와 다른 이유에 대한 설명: https://docs.microsoft.com/en-us/dotnet/standard/base-types/how-do-turkish-culture-information-classes-interpret-strings
- 다양한 문자열 변환 방법: https://www.c-sharpcorner.com/blogs/string-case-conversion-to-uppercase-lowercase-and-title-case1