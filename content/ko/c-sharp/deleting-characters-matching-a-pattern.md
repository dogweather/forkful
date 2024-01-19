---
title:                "패턴에 일치하는 문자 삭제"
html_title:           "Fish Shell: 패턴에 일치하는 문자 삭제"
simple_title:         "패턴에 일치하는 문자 삭제"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# 패턴에 일치하는 문자 삭제하기: C# 가이드 (Deleting Characters Matching a Pattern: A C# Guide)

## 무엇이고 왜 필요한가? (What & Why?)
패턴에 일치하는 문자 삭제는 특정한 형태의 문자열을 찾아 제거하는 프로그래밍 기능입니다. 이는 데이터 정제, 파싱, 일관성 유지 등을 위해 사용한다.

## 어떻게 사용할까? (How to?)
이 함수를 이용해 문자열에서 숫자를 제거해 볼까요? 

```C#
using System.Text.RegularExpressions;

string RemoveNumbers(string input)
{
    return Regex.Replace(input, @"\d", "");
}

string textWithNumbers = "테1스트2txt3";
string textWithoutNumbers = RemoveNumbers(textWithNumbers);
Console.WriteLine(textWithoutNumbers);  // "테스트txt"
```

다음은 모든 띄어쓰기를 제거하는 코드입니다.

```C#
string RemoveSpaces(string input)
{
    return Regex.Replace(input, @"\s", "");
}

string textWithSpaces = "테 스 트 txt";
string textWithoutSpaces = RemoveSpaces(textWithSpaces);
Console.WriteLine(textWithoutSpaces);  // "테스트txt"
```

## 깊이있게 자세히 알아보기 (Deep Dive)
기사의 ASCII 문자를 제거하는 함수는 '70년대에 사용되던 정규 표현식(regex)의 일부 파생 함수로, 문자열을 조작하는데 굉장히 유용했습니다. C#에서는 `Regex.Replace`를 사용해 이 처럼 문자열 조작을 할 수 있습니다.

대안으로는 `StringBuilder` 클래스도 있습니다. 이는 특히 큰 데이터에서 더 효율적일 수 있습니다.

## 참고자료 (See Also)
- Microsoft .NET 정규 표현식 문서: [here](https://docs.microsoft.com/ko-kr/dotnet/standard/base-types/regular-expression-language-quick-reference)
- `Regex.Replace` 메소드: [here](https://docs.microsoft.com/ko-kr/dotnet/api/system.text.regularexpressions.regex.replace?view=netframework-4.8)
- `StringBuilder` 클래스: [here](https://docs.microsoft.com/ko-kr/dotnet/api/system.text.stringbuilder?view=netframework-4.8)