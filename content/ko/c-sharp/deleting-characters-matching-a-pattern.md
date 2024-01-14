---
title:                "C#: 패턴과 일치하는 문자 삭제"
simple_title:         "패턴과 일치하는 문자 삭제"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜?

문자열에서 특정 패턴과 일치하는 문자를 삭제하는 것이 왜 유용한지 궁금할 수 있습니다. 예를 들어, 사용자의 입력에서 특정 문자를 제거하고자 할 때 이 기능을 사용할 수 있습니다.

## 사용 방법

코드 블록을 사용하여 C# 예시와 결과를 제공합니다. 

```C#
string input = "hello123";
string pattern = "[0-9]"; //숫자를 나타내는 정규표현식
string output = Regex.Replace(input, pattern, ""); //문자열에서 숫자를 삭제
Console.WriteLine(output); //결과: hello
```

이 예제에서는 입력된 문자열에서 숫자를 제거하고 나머지만 남게 됩니다. 이를 응용하여 사용자가 원하는 패턴 혹은 문자를 선택적으로 삭제할 수 있습니다. 이를 통해 데이터 정리나 검색 기능 등에 활용할 수 있습니다.

## 딥 다이브

삭제된 문자를 처리하는 메커니즘에 대해 더 자세히 살펴보겠습니다. 문자를 삭제하는 것은 주로 정규표현식을 사용하여 이루어집니다. 정규표현식은 특정 패턴을 가진 문자열을 검색하고 삭제하는 작업에 매우 유용합니다. 또한, 정규표현식은 문자열 처리에 있어서 다양한 기능을 제공하기 때문에 개발자에게 매우 중요한 도구 중 하나입니다.

## 참고 자료

- [C# 정규표현식 - Microsoft 문서](https://docs.microsoft.com/ko-kr/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [트리밍과 정규표현식 관련 포스팅 - velog 사이트](https://velog.io/@dahaha0313/%EC%A0%95%EA%B7%9C%ED%91%9C%ED%98%84%EC%8B%9Dtrim)
- [정규표현식을 사용한 문자열 처리 - Tistory 블로그](https://darock.tistory.com/31)

# 참고 자료