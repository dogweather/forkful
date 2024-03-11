---
date: 2024-01-20 17:58:36.508222-07:00
description: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uBC14\uAFB8\uAE30\uB294 \uBB38\
  \uC790\uC5F4 \uB0B4\uC5D0\uC11C \uD2B9\uC815 \uB2E8\uC5B4\uB098 \uD328\uD134\uC744\
  \ \uCC3E\uACE0 \uBCC0\uACBD\uD558\uB294 \uC791\uC5C5\uC785\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130 \uC870\uC791, \uC124\uC815 \uBCC0\
  \uACBD, \uCF54\uB4DC \uB9AC\uD329\uD1A0\uB9C1 \uB4F1\uC744 \uC704\uD574 \uC774 \uAE30\
  \uB2A5\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-11T00:14:29.445995-06:00'
model: gpt-4-1106-preview
summary: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uBC14\uAFB8\uAE30\uB294 \uBB38\uC790\
  \uC5F4 \uB0B4\uC5D0\uC11C \uD2B9\uC815 \uB2E8\uC5B4\uB098 \uD328\uD134\uC744 \uCC3E\
  \uACE0 \uBCC0\uACBD\uD558\uB294 \uC791\uC5C5\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB4E4\uC740 \uB370\uC774\uD130 \uC870\uC791, \uC124\uC815 \uBCC0\uACBD\
  , \uCF54\uB4DC \uB9AC\uD329\uD1A0\uB9C1 \uB4F1\uC744 \uC704\uD574 \uC774 \uAE30\uB2A5\
  \uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uAC80\uC0C9 \uBC0F \uAD50\uCCB4"
---

{{< edit_this_page >}}

## What & Why? (무엇 그리고 왜?)
텍스트 검색 및 바꾸기는 문자열 내에서 특정 단어나 패턴을 찾고 변경하는 작업입니다. 프로그래머들은 데이터 조작, 설정 변경, 코드 리팩토링 등을 위해 이 기능을 사용합니다.

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
