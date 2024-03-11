---
date: 2024-01-20 17:37:37.469214-07:00
description: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD55C\uB2E4\
  \uB294 \uAC83\uC740, \uCEF4\uD4E8\uD130\uAC00 \uC774\uD574\uD560 \uC218 \uC788\uB294\
  \ \uB0A0\uC9DC \uD615\uC2DD\uC744 \uC0AC\uB78C\uC774 \uC77D\uC744 \uC218 \uC788\uB294\
  \ \uD14D\uC2A4\uD2B8\uB85C \uBC14\uAFB8\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uBCF4\uD1B5 \uB85C\uAE45, \uC0AC\uC6A9\
  \uC790 \uC778\uD130\uD398\uC774\uC2A4 \uB610\uB294 \uB370\uC774\uD130 \uAD50\uD658\
  \uC744 \uC704\uD574 \uC774 \uC791\uC5C5\uC744 \uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: '2024-03-11T00:14:29.487838-06:00'
model: gpt-4-1106-preview
summary: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD55C\uB2E4\uB294\
  \ \uAC83\uC740, \uCEF4\uD4E8\uD130\uAC00 \uC774\uD574\uD560 \uC218 \uC788\uB294\
  \ \uB0A0\uC9DC \uD615\uC2DD\uC744 \uC0AC\uB78C\uC774 \uC77D\uC744 \uC218 \uC788\uB294\
  \ \uD14D\uC2A4\uD2B8\uB85C \uBC14\uAFB8\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4\
  . \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uBCF4\uD1B5 \uB85C\uAE45, \uC0AC\uC6A9\
  \uC790 \uC778\uD130\uD398\uC774\uC2A4 \uB610\uB294 \uB370\uC774\uD130 \uAD50\uD658\
  \uC744 \uC704\uD574 \uC774 \uC791\uC5C5\uC744 \uD569\uB2C8\uB2E4."
title: "\uB0A0\uC9DC\uB97C \uBB38\uC790\uC5F4\uB85C \uBCC0\uD658\uD558\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
날짜를 문자열로 변환한다는 것은, 컴퓨터가 이해할 수 있는 날짜 형식을 사람이 읽을 수 있는 텍스트로 바꾸는 것을 말합니다. 프로그래머들은 보통 로깅, 사용자 인터페이스 또는 데이터 교환을 위해 이 작업을 합니다.

## How to: (어떻게 하나요?)
PowerShell에서 날짜를 문자열로 변환하려면 `Get-Date` cmdlet과 함께 `-Format` 매개 변수를 사용하거나 `ToString()` 메서드를 사용합니다. 다음은 예제입니다.

```PowerShell
# 현재 날짜와 시간을 기본 형식으로 가져오기
$CurrentDate = Get-Date
$CurrentDate.ToString()

# 사용자 정의 형식으로 날짜와 시간을 문자열로 변환
$DateFormat = 'yyyy-MM-dd HH:mm:ss'
$CurrentDate.ToString($DateFormat)

# '-Format' 매개 변수를 사용하여 날짜 형식 지정
$FormattedDate = Get-Date -Format $DateFormat
$FormattedDate
```

출력 예:
```
2023-04-05 14:20:36
2023-04-05 14:20:36
```

## Deep Dive (심층 탐구)
날짜 형식 변환은 데이터 처리의 초기 단계부터 존재해왔습니다. PowerShell에서는 다양한 형식을 지원해 표준 ISO 형식부터 사용자 정의 형식까지 가능합니다. `.NET`의 `DateTime` 타입을 기반으로 하며, PowerShell은 이를 확장하여 더 쉽게 작업할 수 있게 해줍니다.

그 외에도, `[datetime]::Parse()`, `[datetime]::ParseExact()`, `[datetime]::TryParse()`, `[datetime]::TryParseExact()`와 같은 메서드를 이용할 수도 있습니다. 이들 메서드는 더 복잡한 날짜 문자열을 `DateTime` 객체로 변환할 때 사용됩니다.

선택적으로 `culture` 정보를 명시하여 언어와 지역에 맞는 날짜 형식을 지정할 수도 있습니다. 이는 다국어 환경에서 중요한 부분입니다.

## See Also (참고 자료)
- [날짜와 시간 패턴으로 문자열 서식 지정](https://docs.microsoft.com/ko-kr/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [Get-Date Cmdlet 공식 문서](https://docs.microsoft.com/ko-kr/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
