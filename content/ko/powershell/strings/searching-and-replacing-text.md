---
date: 2024-01-20 17:58:36.508222-07:00
description: "How to: (\uBC29\uBC95:) \uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uBC14\
  \uAFB8\uAE30\uB294 \uBE44\uB2E8 \uCD5C\uC2E0\uB9CC\uC758 \uD544\uC694\uC131\uC740\
  \ \uC544\uB2D9\uB2C8\uB2E4. \uCD08\uAE30 \uCEF4\uD4E8\uD305 \uC774\uB798\uB85C,\
  \ \uB370\uC774\uD130\uB294 \uBB38\uC790\uC5F4\uB85C \uB9CE\uC774 \uD45C\uD604\uB418\
  \uC5B4 \uC654\uC2B5\uB2C8\uB2E4. \uB530\uB77C\uC11C, \uCD08\uAE30 \uC720\uD2F8\uB9AC\
  \uD2F0 \uD504\uB85C\uADF8\uB7A8\uC5D0\uC11C\uBD80\uD130 \uD574\uB2F9 \uAE30\uB2A5\
  \uC744 \uD544\uC218\uC801\uC73C\uB85C \uD3EC\uD568\uD588\uC2B5\uB2C8\uB2E4. PowerShell\uC740\
  \ '-replace' \uC5F0\uC0B0\uC790\uC640\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.184419-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95:) \uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uBC14\uAFB8\uAE30\
  \uB294 \uBE44\uB2E8 \uCD5C\uC2E0\uB9CC\uC758 \uD544\uC694\uC131\uC740 \uC544\uB2D9\
  \uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4"
weight: 10
---

## How to: (방법:)
```PowerShell
# 텍스트 내 단어 'apple'을 'orange'로 바꾸기
$text = "I have 5 apples"
$updatedText = $text -replace 'apple', 'orange'
Write-Output $updatedText # I have 5 oranges
```

```PowerShell
# 정규 표현식 사용하여 날짜 형식 바꾸기
$dateText = "Today is 2023-01-25"
$updatedDateText = $dateText -replace '(\d{4})-(\d{2})-(\d{2})', '$3/$2/$1'
Write-Output $updatedDateText # Today is 25/01/2023
```

## Deep Dive (심층 분석)
텍스트 검색 및 바꾸기는 비단 최신만의 필요성은 아닙니다. 초기 컴퓨팅 이래로, 데이터는 문자열로 많이 표현되어 왔습니다. 따라서, 초기 유틸리티 프로그램에서부터 해당 기능을 필수적으로 포함했습니다. PowerShell은 '-replace' 연산자와 함께 정규 표현식을 지원하여 유연한 문자열 조작을 제공합니다. 윈도우 뿐만 아니라 리눅스와 macOS에서도 사용 가능합니다. 

대체하는 방법은 다양합니다. 예를 들어, .NET의 `String.Replace()` 메서드나, 텍스트 편집기의 찾기 및 바꾸기 기능도 있습니다. PowerShell의 `-replace`는 자동화 스크립트에서 매우 유용합니다.

## See Also (참고 자료)
- [Microsoft's Official PowerShell Documentation](https://docs.microsoft.com/powershell/)
- [Regular Expressions Quick Start](https://www.regular-expressions.info/quickstart.html)
- [About Comparison Operators (including -replace)](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7.1)
