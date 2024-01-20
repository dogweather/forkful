---
title:                "두 날짜 비교하기"
html_title:           "C#: 두 날짜 비교하기"
simple_title:         "두 날짜 비교하기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇이며 왜필요한가?

날짜 비교는 두 날짜 간의 차이를 확인하는 일입니다. 프로그래머들이 이 기능을 사용하는 주요 이유는 특정 기간동안 어떤 작업을 스케줄링하거나, 두 개의 사건이 얼마나 떨어져 있는지를 확인하기 위해서입니다.

## 사용방법:

PowerShell을 사용해서 두 날짜를 비교하는 방법은 아래 코드를 참고해주세요.

```PowerShell
$date1 = Get-Date
$date2 = Get-Date -Date '2023-12-01'
$diff = New-TimeSpan -Start $date1 -End $date2
$diff.Days
```

위의 코드는 $date1이 현재 날짜를, $date2는 2023년 12월 1일을 표현하는 예시입니다. 마지막 줄에서 Days 속성을 호출하면 두 날짜 간의 차이를 일단위로 확인할 수 있습니다.

## 깊이있게 알아보기:

날짜 비교는 프로그래밍의 처음부터 존재하던 기능 중 하나입니다. 이미 고안된 수많은 방법들이 있지만, PowerShell에서는 New-TimeSpan 커맨드를 이용하여 간단하게 계산 가능합니다.  

하나의 대안적인 방법으로, .net의 DateTime 구조를 사용해 두 날짜를 직접 비교할 수도 있습니다.

```PowerShell
$date1 = [DateTime]::Now  
$date2 = [DateTime]'2023-12-01'
$diff = $date2 - $date1
$diff.Days
```

위 코드에서는 .NET의 DateTime 객체를 이용하여 날짜 차이를 계산하였습니다.

## 참조 링크:

날짜 비교에 대한 추가적인 정보는 아래의 링크를 참조하시면 좋을 것 같습니다.

1. [Microsoft Official Documentation on New-TimeSpan](https://docs.microsoft.com/ko-kr/powershell/module/microsoft.powershell.utility/new-timespan?view=powershell-7.2)
2. [Microsoft Official Documentation on Get-Date](https://docs.microsoft.com/ko-kr/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.2)
3. [Stackoverflow Discussion on Comparing Dates in PowerShell](https://stackoverflow.com/questions/2743732/how-to-compare-two-dates-with-powershell)