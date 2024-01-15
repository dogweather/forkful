---
title:                "문자열을 소문자로 변환하기"
html_title:           "C#: 문자열을 소문자로 변환하기"
simple_title:         "문자열을 소문자로 변환하기"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜

문자열을 소문자로 변환하는 것이 왜 필요한지 궁금하신가요? 문자열을 처리하는 많은 작업에서 대소문자를 구분할 필요가 없거나, 소문자로 통일해야 할 때가 있기 때문입니다.

## 어떻게

먼저, `ToLower()` 메서드를 사용하여 문자열을 소문자로 변환하는 방법을 알아보겠습니다. 아래의 예제 코드는 입력받은 문자열을 소문자로 변환한 뒤 출력하는 예제입니다.

```C#
string input = "Hello World!";
string lowerCase = input.ToLower();
Console.WriteLine(lowerCase);
```
출력 결과: `hello world!`

또 다른 방법은 `ToLowerInvariant()` 메서드를 사용하는 것입니다. 이 메서드는 문화권에 따른 대소문자 규칙을 무시하고 소문자로 변환해줍니다. 예제 코드는 아래와 같습니다.

```C#
string input = "Hello World!";
string lowerCase = input.ToLowerInvariant();
Console.WriteLine(lowerCase);
```
출력 결과: `hello world!`

마지막으로, LINQ를 활용하여 문자열의 모든 문자를 소문자로 변환할 수도 있습니다. 아래의 예제 코드는 `Select()` 메서드를 사용하여 문자열의 각 문자를 `char.ToLower()` 메서드를 이용해 소문자로 변환한 뒤 문자열로 다시 합치는 예제입니다.

```C#
string input = "Hello World!";
string lowerCase = string.Join("", input.Select(c => char.ToLower(c)));
Console.WriteLine(lowerCase);
```
출력 결과: `hello world!`

## 딥 다이브

.NET Framework에서 문자열을 소문자로 변환할 때는 기본적으로 현재 문화권의 대소문자 규칙을 따르게 됩니다. 만약 다른 문화권의 규칙을 따르고 싶다면 `ToLower()` 메서드 대신 `ToLower(CultureInfo)` 메서드를 사용할 수 있습니다.

또한, `ToLower()` 메서드는 문자열의 복사본을 반환하는 반면, `ToLowerInvariant()` 메서드는 원본 문자열을 변경하지 않고 새로운 문자열을 생성하여 반환합니다. 이는 메모리 관리 측면에서 주의해야 할 점입니다.

## 관련 자료

- [.NET Framework에서 문자열 변환 방법](https://docs.microsoft.com/ko-kr/dotnet/api/system.string.tolower?view=netframework-4.8)
- [문자열 관련 LINQ 메서드](https://docs.microsoft.com/ko-kr/dotnet/csharp/programming-guide/concepts/linq/string-related-linq-methods)
- [정규식을 사용한 문자열 전처리 방법](https://www.c-sharpcorner.com/blogs/working-with-regular-expression-in-c-sharp)