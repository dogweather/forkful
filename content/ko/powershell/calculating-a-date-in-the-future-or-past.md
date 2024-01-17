---
title:                "미래나 과거 날짜 계산하기"
html_title:           "PowerShell: 미래나 과거 날짜 계산하기"
simple_title:         "미래나 과거 날짜 계산하기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 무엇이며, 왜?: 

먼저, 일정한 날짜를 미래나 과거로 계산하는 것은 일반적인 프로그래밍 작업입니다. 프로그래머들은 이런 계산을 활용하여 특정 날짜에 따른 작업을 수행하고, 날짜와 시간 기반의 프로그램을 만들 수 있습니다.

## 하는 방법: 

PowerShell은 매우 강력한 날짜 계산 기능을 가지고 있습니다. 예를 들어, 7일 후의 날짜를 계산하려면 `Get-Date` 명령어와 `AddDays` 메서드를 사용합니다. 아래 예시를 참고하세요.

```PowerShell
(Get-Date).AddDays(7)
```

결과는 다음과 같이 나옵니다.
```
Monday, January 11, 2021 12:50:34 PM
```

이외에도 PowerShell에서는 다양한 날짜 계산 메서드를 제공합니다. `AddYears`, `AddMonths`, `AddHours` 등의 메서드를 사용하여 특정 날짜에 대한 연산을 수행할 수 있습니다. 자세한 내용은 매뉴얼을 참고하세요.

## 더 알아보기: 

날짜 계산은 프로그래밍이나 컴퓨팅 분야에서 중요한 역할을 합니다. 미래나 과거 날짜로 계산하는 방법은 크게 두 가지로 나뉘게 됩니다. 첫 번째는 달력을 사용하는 방법이고, 두 번째는 숫자 차이를 계산하는 방법입니다. PowerShell에서도 두 가지 방법을 모두 제공합니다.

예를 들어, 2020년 1월 1일부터 2020년 12월 31일까지의 총 일 수를 알고 싶다면 `New-TimeSpan` 명령어를 사용하여 날짜 간의 차이를 계산할 수 있습니다. 또는 특정 달의 첫 번째 날을 찾고 싶다면 `Get-Date` 명령어와 `AddDays` 메서드를 사용하여 계산할 수 있습니다.

더 자세한 내용은 관련된 문서와 매뉴얼을 참고하세요.

## 더 알아보기: 

PowerShell의 날짜 계산 기능은 프로그래밍에 있어서 매우 중요합니다. 자세한 내용을 알고 싶다면 아래 링크를 확인해보세요. 

- 매뉴얼: https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/add-days?view=powershell-7
- 블로그: https://devblogs.microsoft.com/scripting/use-powershell-to-add-x-number-of-days-to-a-date/

원문 링크: https://docs.microsoft.com/ko-kr/powershell/