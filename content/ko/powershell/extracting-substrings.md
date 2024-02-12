---
title:                "부분 문자열 추출"
aliases:
- ko/powershell/extracting-substrings.md
date:                  2024-01-20T17:46:24.192808-07:00
model:                 gpt-4-1106-preview
simple_title:         "부분 문자열 추출"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
문자열에서 부분 문자열을 추출한다는 건, 큰 문자열 안에서 원하는 작은 조각을 뽑아내는 작업입니다. 코드를 통해 데이터를 분해하고, 필요한 정보만 골라내기 위해 이 과정을 사용합니다.

## How to: (어떻게 하나요?)
```PowerShell
# 기본적인 Substring 사용
$string = "Hello, PowerShell!"
$substring = $string.Substring(7, 11)
Write-Output $substring # PowerShell!

# 문자열 범위를 사용한 부분 추출
$range = 7..17
$substring = $string[$range]
Write-Output $substring # PowerShell!

# -split을 사용해 문자열 자르기
$parts = $string -split ', '
Write-Output $parts[1] # PowerShell!

# 정규 표현식과 -match를 이용한 부분 추출
$string -match "PowerShell"
Write-Output $matches[0] # PowerShell
```

## Deep Dive (심층 분석)
부분 문자열을 추출하는 방식은 컴퓨터 프로그래밍의 초기부터 있어왔습니다. PowerShell에서는 `.Substring()`, 문자열 범위, `-split`, `-match`와 같은 다양한 방법으로 부분 문자열 추출을 수행할 수 있습니다. `.Substring()`은 기본적이고 직관적인 방법이며, `문자열[인덱스..인덱스]` 범위를 통해서도 가능합니다. `-split` 분할자는 문자열을 정해진 구분자로 나누어 배열로 반환하고, `-match`와 정규 표현식은 특정 패턴에 일치하는 부분을 찾을 때 유용합니다. PowerShell은 .NET 프레임워크 위에서 구동되기 때문에 .NET의 강력한 문자열 처리 기능을 그대로 활용할 수 있습니다.

## See Also (참고 자료)
- [about_Split](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_split)
- [about_Regular_Expressions](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_regular_expressions)
- [String class on MSDN](https://docs.microsoft.com/dotnet/api/system.string)

*참고: 위의 웹사이트들은 영어로 작성된 자료입니다.*
