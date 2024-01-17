---
title:                "패턴과 일치하는 문자 삭제하기"
html_title:           "C#: 패턴과 일치하는 문자 삭제하기"
simple_title:         "패턴과 일치하는 문자 삭제하기"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

캐릭터 패턴을 일치하는 문자열을 삭제하기는 프로그래머들이 자주하는 작업입니다. 이 작업은 특정한 문자열을 찾아서 삭제하는 것인데, 이를 통해 코드를 더 깔끔하고 효율적으로 만들 수 있습니다.

## 방법:

```C#
string input = "Korean Programmers are great!";
// "a"로 끝나는 문자열은 삭제됩니다.
string output = Regex.Replace(input, @"a$", "");
Console.WriteLine(output); // 결과값: "Korean Programmrs re gret!"
```

## 깊게 파고들기:

이 작업은 보통 특정한 문자열 대신 패턴을 사용하기 때문에 더 깊게 파고들 수 있습니다. 예를 들어, 정규 표현식을 사용하면 더 복잡한 패턴도 가능합니다. 또 다른 대안으로는 문자열을 대체하는 방법이 있습니다. 또는 이 작업을 수행하는 다른 언어나 프레임워크들도 있습니다.

## 더 읽어보기:

- [정규 표현식 사용법](https://www.hypertextual.com/2015/05/regex-regex-regex/)
- [.NET Framework 문자열 처리 방법](https://docs.microsoft.com/ko-kr/dotnet/standard/base-types/regular-expression-language-quick-reference#regexoptions)
- [다른 언어의 문자열 매칭 방법](https://www.geeksforgeeks.org/program-to-find-strings-that-match-the-given-regular-expression-from-a-list/)