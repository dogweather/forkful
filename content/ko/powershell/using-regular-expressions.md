---
title:                "정규 표현식 활용하기"
html_title:           "Arduino: 정규 표현식 활용하기"
simple_title:         "정규 표현식 활용하기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)

정규 표현식은 문자열에서 패턴을 찾기 위한 강력한 도구입니다. 프로그래머들은 코드의 중복을 피하고, 복잡한 문자열 처리 작업을 단순화하기 위해 이를 사용합니다.

## How to: (방법)

```PowerShell
# 문자열에서 숫자 찾기
$string = '오늘은 2023년 3월 15일입니다.'
$pattern = '\d+'
[regex]::Matches($string, $pattern) | ForEach-Object { $_.Value }

# 결과: 2023, 3, 15

# 이메일 유효성 검사
$email = 'example@domain.com'
$pattern = '^\S+@\S+\.\S+$'
if ($email -match $pattern) {
    "유효한 이메일입니다."
} else {
    "유효하지 않은 이메일입니다."
}

# 결과: 유효한 이메일입니다.
```

## Deep Dive (심화 학습)

정규 표현식은 1950년대 초반, 수학자 스티븐 클리니가 시작한 개념입니다. Alternatives로는 `String.Contains`, `String.StartsWith`, `String.EndsWith` 같은 내장 함수들이 있으나 이들은 정규 표현식만큼 유연하지 않습니다. PowerShell에서는 `System.Text.RegularExpressions.Regex` 클래스를 사용하여 구현합니다. 이 클래스는 .NET Framework의 일부로 성능이 우수하며, 복잡한 문자열 패턴을 처리할 수 있습니다.

## See Also (참조)

- [정규 표현식 - MSDN](https://docs.microsoft.com/ko-kr/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [정규 표현식 테스트 사이트 - Regex101](https://regex101.com/)
