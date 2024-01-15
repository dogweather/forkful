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

##

## 왜

패턴과 일치하는 문자를 삭제하는 작업을 하게 될 이유는 무엇일까요? 한 번에 두 가지 이유를 말씀 드리겠습니다. 첫 번째로는 데이터를 정리하거나 변경하는 경우에 유용합니다. 두 번째로는 보안 상의 이유로 유해한 문자를 삭제하는 경우입니다.

## 하우투

패턴과 일치하는 문자를 삭제하는 가장 간단한 방법은 문자열 메소드 중 하나인 `Replace`를 사용하는 것입니다. 예를 들어, 문자열에서 모음만 삭제하고 싶다면 다음과 같이 작성할 수 있습니다.

```C#
string str = "Hello World";
string vowels = "aeiou";

foreach (char c in vowels)
{
    str = str.Replace(c.ToString(), "");
}

Console.WriteLine(str); // Prints "Hll Wrld"
```

## 딥 다이브

삭제하는 패턴을 조금 더 유연하게 바꿀 수도 있습니다. 예를 들어, 모든 숫자를 삭제하고 싶을 때도 있을 것입니다. 이 경우에는 정규표현식을 사용하는 것이 좋습니다. 정규표현식은 일치하는 패턴을 정의하는 문자열 패턴입니다. C#에서는 `Regex` 클래스를 사용하여 정규표현식을 직접 작성하거나 이미 만들어진 패턴을 사용할 수 있습니다.

```C#
string str = "123 456 James";
string pattern = @"\d"; // This will match any digit

str = Regex.Replace(str, pattern, "");

Console.WriteLine(str); // Prints " James"
```

정규표현식은 매우 강력하지만 복잡하기 때문에 익숙해지기까지 시간이 걸릴 수 있습니다. 하지만 익숙해지면 텍스트를 다루는 여러 가지 작업에서 유용하게 사용할 수 있습니다.

## 봐도 좋아요

- [C# Replace 메소드를 사용하여 문자열에서 문자 삭제하기](https://docs.microsoft.com/ko-kr/dotnet/csharp/how-to/replace-characters-in-a-string)
- [C# 정규표현식 사용하기](https://docs.microsoft.com/ko-kr/dotnet/standard/base-types/regular-expression-language-quick-reference)