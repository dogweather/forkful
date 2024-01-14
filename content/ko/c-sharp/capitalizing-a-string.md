---
title:                "C#: 문자열 대문자로 변환하기"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 왜

문자열에 대문자를 사용하는 일에 참여하는 이유는 무엇일까요? 한-영 번역기를 사용하거나 키보드를 변경하는 등의 경우에는 일반적으로 문자열에 대문자를 사용하는 것이 더 효율적일 수 있습니다.

## 방법

아래 코드 블록에는 C# 언어로 작성된 예제와 해당 예제의 출력 결과가 포함되어 있습니다.

```C#
string input = "hello world";
string capitalized = input.ToUpper();
Console.WriteLine(capitalized);
```

출력 결과: HELLO WORLD

## 깊이 파고들기

문자열을 대문자로 바꾸기 위해 사용된 `ToUpper()` 메서드는 단순히 문자열의 모든 문자를 대문자로 바꾸는 것이 아닙니다. 실제로는 문자의 아스키 코드 값을 기준으로 대소문자를 구분하고 대문자의 아스키 코드 값을 반환합니다. 따라서 영문자 외의 다른 문자는 변경되지 않습니다.

## 참고

- [C# 문자열의 대문자로 변경하기](https://www.c-sharpcorner.com/blogs/converting-lowercase-character-to-uppercase-using-tostring-method-in-c-sharp1)
- [스트링 내 문자를 소문자에서 대문자로 변환](https://stackoverflow.com/questions/2443085/convert-string-to-lower-case-capital-first-letter-each-word)