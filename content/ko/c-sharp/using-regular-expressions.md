---
title:                "정규식 사용하기"
html_title:           "C#: 정규식 사용하기"
simple_title:         "정규식 사용하기"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

정규식(Regular Expression)은 특정한 패턴을 가진 문자열을 찾거나 다룰 때 사용하는 표현 방식입니다. 프로그래머들은 정규식을 사용하여 다양한 문자열을 더 간편하고 빠르게 처리할 수 있습니다.

## 하는 법:

```C#
// 문자열에서 숫자만 추출하는 예제
string input = "Hello 123 World";
string pattern = @"\d+";  // 숫자 하나 이상을 의미하는 정규식

Match match = Regex.Match(input, pattern);
if(match.Success)
{
    Console.WriteLine("숫자만 출력: " + match.Value); // 숫자만 출력: 123
}
```

## 깊은 곳:

정규식은 1950년대부터 사용되어온 방식으로써, 문자열을 처리할 때 유용하게 쓰이고 있습니다. 정규식 외에도 문자열을 다룰 때 효율적인 방법들이 있지만, 정규식은 강력하면서도 다양한 패턴을 처리할 수 있다는 장점이 있습니다. 정규식을 구현할 때는 사용하는 프로그래밍 언어에 따라 다소 차이가 있을 수 있으므로, 잘 익히고 활용하는 것이 중요합니다.

## 관련 자료:

- [C# 정규식 예제](https://docs.microsoft.com/ko-kr/dotnet/standard/base-types/regular-expression-examples)
- [정규식 테스트 사이트](https://regex101.com/)