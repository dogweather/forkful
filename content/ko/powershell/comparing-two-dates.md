---
title:                "두 날짜를 비교하는 방법"
html_title:           "PowerShell: 두 날짜를 비교하는 방법"
simple_title:         "두 날짜를 비교하는 방법"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?: 
두 날짜를 비교하는 것은 프로그래머가 하게 되는 작업입니다. 이를 통해 특정 날짜를 전달받아 다른 날짜와 비교하여 필요한 조건을 만족하는지 확인할 수 있습니다.

## 방법:
```powershell
# 두 날짜를 변수로 지정
$date1 = Get-Date -Year 2021 -Month 05 -Day 15
$date2 = Get-Date -Year 2021 -Month 05 -Day 17

# 비교 연산자를 사용하여 날짜 비교
if($date1 -gt $date2){
    Write-Output "첫 번째 날짜가 두 번째 날짜보다 늦습니다."
}
elseif($date1 -lt $date2){
    Write-Output "첫 번째 날짜가 두 번째 날짜보다 이전입니다."
}
else{
    Write-Output "두 날짜가 같습니다."
}
```

```powershell
# 날짜 비교 결과
첫 번째 날짜가 두 번째 날짜보다 이전입니다.
```

## 깊게 들어가기:
(1) 두 날짜를 비교하는 방법은 시간의 흐름을 파악하고 다양한 작업을 수행하는 데 중요한 역할을 합니다. (2) PowerShell에서는 비교 연산자 외에도 날짜 형식을 파싱하고 계산할 수 있는 다양한 기능을 제공합니다. (3) 두 날짜를 정확히 비교하기 위해서는 날짜 형식의 표기법과 비교 연산자의 사용 방법에 대해 자세히 알아야 합니다.

## 참고 자료:
- [PowerShell Get-Date cmdlet 문서](https://docs.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Utility/Get-Date?view=powershell-7.1)
- [PowerShell Comparison Operators 문서](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7.1)
- [자바와 PowerShell 날짜 비교 예제](https://www.javatpoint.com/powershell-datetime-comparison)