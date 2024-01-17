---
title:                "문자열 대문자로 변경하기"
html_title:           "C#: 문자열 대문자로 변경하기"
simple_title:         "문자열 대문자로 변경하기"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?

문자열을 대문자로 변환하는 것은 문자열을 전부 대문자로 만드는 것을 말합니다. 프로그래머들은 일반적으로 문자열을 대문자로 변환하는 이유는 사용자의 입력을 통일하고 데이터 처리를 쉽게하기 위해서입니다.

## 방법:

```C#
// 입력된 문자열을 대문자로 변환하는 예시
string input = "hello world";
string output = input.ToUpper();

// 출력: "HELLO WORLD"
```

## 깊이있게 살펴보기:

이전에 문자열을 대문자로 변환하기 위해서는 대문자로 변환하는 라이브러리를 사용하거나 수동으로 문자열의 문자 하나하나를 대문자로 변경하는 방법이 있었습니다. 하지만 C#에서는 더욱 쉽게 문자열을 대문자로 변환할 수 있도록 `ToUpper()` 메소드를 제공합니다. 이를 통해 프로그래머들은 더 효율적이고 간편하게 문자열을 대문자로 변환할 수 있습니다.

## 참고 자료:

- [C# ToUpper() 메소드 문서](https://docs.microsoft.com/en-us/dotnet/api/system.string.toupper)
- [Java로 문자열 대문자로 변환하기](https://www.java67.com/2017/10/how-to-convert-string-to-upper-case-in-java-example.html)
- [Python으로 문자열 대문자로 변환하기](https://www.geeksforgeeks.org/python-string-uppercase-method/)