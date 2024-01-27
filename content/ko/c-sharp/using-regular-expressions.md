---
title:                "정규 표현식 활용하기"
date:                  2024-01-19
html_title:           "Arduino: 정규 표현식 활용하기"
simple_title:         "정규 표현식 활용하기"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
(무엇이며 왜 사용하는가?)
정규 표현식은 문자열 패턴 매칭을 위한 도구입니다. 프로그래머들이 데이터 검증, 검색, 추출, 대체 작업을 쉽고 효율적으로 하기 위해 사용합니다.

## How to:
(어떻게 사용하는가?)
```C#
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        // 이메일 유효성 검사
        string pattern = @"\w+@\w+\.\w+";
        string testEmail = "example@email.com";
        
        bool isValidEmail = Regex.IsMatch(testEmail, pattern);
        Console.WriteLine("이메일 유효: " + isValidEmail);  // 출력: 이메일 유효: True

        // 문자열에서 숫자 추출
        string data = "Price: 1234 Won";
        string numPattern = @"\d+";
        
        MatchCollection matches = Regex.Matches(data, numPattern);
        foreach (Match match in matches)
        {
            Console.WriteLine("숫자 추출: " + match.Value);  // 출력: 숫자 추출: 1234
        }
    }
}
```

## Deep Dive:
(심층 분석)
정규 표현식은 1950년대 수학자 스티븐 클리니가 발명했습니다. 널리 쓰이는 대안으로는 문자열 함수들이 있지만, 정규 표현식이 훨씬 강력합니다. C#에서는 `System.Text.RegularExpressions` 네임스페이스 안의 `Regex` 클래스를 통해 구현됩니다. 속도가 중요하다면, 정규 표현식의 컴파일 옵션을 고려할 수 있습니다.

## See Also:
(더 보기)
- [Microsoft Docs: .NET 정규 표현식](https://docs.microsoft.com/ko-kr/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [Regex101: 정규 표현식 테스트](https://regex101.com/)
- [Stack Overflow: 정규 표현식 질문](https://stackoverflow.com/questions/tagged/regex)
