---
title:                "현재 날짜 가져오기"
date:                  2024-01-20T15:16:09.895215-07:00
simple_title:         "현재 날짜 가져오기"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
날짜를 확인하는 것은 단순히 현재 날짜와 시간을 얻는 과정입니다. 프로그래머들은 로깅, 타임스탬프, 기간 측정 등을 위해 이를 사용합니다.

## How to: (어떻게 하나요?)
실제 코드를 통해 현재 날짜를 얻는 법을 배워봅시다.

```PowerShell
# 현재 날짜와 시간을 얻습니다
Get-Date

# 현재 날짜를 'yyyy-MM-dd' 형식으로 출력합니다
Get-Date -Format "yyyy-MM-dd"

# 현재 시간을 'HH:mm:ss' 형식으로 출력합니다
Get-Date -Format "HH:mm:ss"
```

실행 결과는 이렇게 나옵니다:

```PowerShell
Sunday, April 2, 2023 12:05:28 PM
2023-04-02
12:05:28
```

## Deep Dive (심층 분석)
`Get-Date` 명령은 PowerShell에서 처음 시작할 때부터 있었습니다. `.NET` 기반으로, 정확하고 다양한 날짜 형식을 지원합니다. 

이 외에도, `[System.DateTime]` 타입을 직접 사용하여 날짜와 시간 정보를 얻을 수 있습니다.

```PowerShell
[System.DateTime]::Now
```

다른 언어나 쉘에 비해 PowerShell은 날짜 조작이 간편합니다. 예를 들어, 하루 전의 날짜를 구하고 싶을 때:

```PowerShell
(Get-Date).AddDays(-1)
```

이 기능은 스크립트 내에서 일정 관리, 경과 시간 계산 등에 유용하게 쓰입니다.

## See Also (관련 자료)
- [Get-Date (Microsoft Docs)](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date)
- [DateTime Struct (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime)
