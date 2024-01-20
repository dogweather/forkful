---
title:                "정규 표현식 사용하기"
html_title:           "Bash: 정규 표현식 사용하기"
simple_title:         "정규 표현식 사용하기"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?

정규 표현식은 문자열의 특정 패턴을 찾거나 수정하는 유용한 도구입니다. 이를 사용하면 복잡한 텍스트 처리 및 검증 작업을 간단하게 해결할 수 있습니다.

## 실제 코드:

```C#
using System;
using System.Text.RegularExpressions;

class Program {
    static void Main() {
        // 입력 문자열 
        string input = "Hello, my phone number is 987-654-3210.";
        
        // 정규표현식을 사용하여 전화번호 패턴 찾기
        string pattern = @"\d{3}-\d{3}-\d{4}";
        Match m = Regex.Match(input, pattern);

        if (m.Success) {
            Console.WriteLine("Match found: " + m.Value);
        }
    }
}
```
이 코드를 실행하면 출력결과는 `Match found: 987-654-3210` 입니다.

## 심층 분석:

(1) **역사적 배경**: 정규 표현식은 1950년대에 수학자 스티븐 콜 클레니에 의해 개발되었습니다. 처음에는 문자열 처리를 위한 유용한 수학적 표현식이었지만, 이후에 컴퓨터 공학 분야에서 널리 이용되기 시작했습니다.

(2) **대체 수단**: 정규 표현식 라이브러리가 없는 경우 또는 더 간단한 작업의 경우, 문자열 메서드를 직접 이용해도 됩니다. 예를 들어, 'StartsWith', 'EndsWith', 'IndexOf', 'LastIndexOf' 등의 메서드가 있습니다.

(3) **구현 세부사항**: C#에서는 `System.Text.RegularExpressions` 네임스페이스에 있는 `Regex` 클래스를 사용하여 정규 표현식을 구현합니다. `Match`, `Matches`, `IsMatch`, `Replace`, `Split` 등 여러 메서드들이 제공됩니다.

## 참고:

- [Microsoft Official Documentation: Regular Expressions](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [RegexOne - Learn Regular Expressions](https://regexone.com/)
- [StackOverflow: Understanding Regular Expressions](https://stackoverflow.com/questions/4736/understanding-regular-expressions)