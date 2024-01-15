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

## 왜 정규식을 사용할까요?

정규식은 문자열에서 특정 패턴을 찾거나 대체하기 위해 사용됩니다. 예를 들어, 이메일 주소나 전화번호와 같은 형식의 문자열을 찾거나, 문자열에서 특정 단어를 대체하거나 제거할 수 있습니다.

따라서 정규식은 문자열 처리 작업을 효율적으로 수행하기 위해 빈번하게 사용됩니다.

## 사용법

정규식을 사용하기 위해 우선 C#의 `Regex` 클래스를 이용합니다.

```C#
using System.Text.RegularExpressions;

// 정규식 패턴 정의
string pattern = @"[aeiou]"; // 모음을 찾는 패턴

// 문자열 생성
string input = "Regular expressions are powerful tools for string manipulation.";

// 정규식 검색
MatchCollection matches = Regex.Matches(input, pattern);

// 결과 출력
foreach (Match match in matches)
{
    Console.WriteLine(match.Value); // "e", "u", "e", "o", "e", "e", "a", "i", "a", "i", "u", "a", "i", "u", "i", "o", "a", "i", "u"
}
```

위의 예제에서는 `Regex.Matches()` 메서드를 사용하여 정규식에 맞는 모든 부분 문자열을 찾아내고, `MatchCollection` 객체에 저장합니다. 그 후 `foreach`문을 사용하여 결과를 출력합니다.

## 더 깊이 알아보기

정규식은 다양한 패턴을 정의할 수 있으며, 이를 조합하여 더 복잡한 문자열 처리를 가능하게 합니다.

여러 개의 패턴을 적용할 경우에는 `RegexOptions` 열거형을 사용하여 정규식을 더욱 세밀하게 제어할 수 있습니다. 또한, 그룹화와 역참조를 활용하여 정규식을 더욱 강력하게 만들 수 있습니다.

더 많은 정보는 마이크로소프트의 [C# 정규식 문서](https://docs.microsoft.com/ko-kr/dotnet/standard/base-types/regular-expression-language-quick-reference)를 참고하시기 바랍니다.

## 같이 보기

- [MDN 정규식 가이드](https://developer.mozilla.org/ko/docs/Web/JavaScript/Guide/Regular_Expressions)
- [정규식 표현 검증기](https://regexr.com/)