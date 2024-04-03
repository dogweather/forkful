---
date: 2024-01-26 01:11:20.007326-07:00
description: "\uBC29\uBC95: \uB450 \uC22B\uC790\uC758 \uD569\uC744 \uACC4\uC0B0\uD558\
  \uB294 \uD568\uC218\uB97C \uC791\uC131\uD574 \uBCF4\uACA0\uC2B5\uB2C8\uB2E4. \uAC04\
  \uB2E8\uD558\uC9C0\uB9CC, \uC911\uC694\uD55C \uC810\uC744 \uC798 \uBCF4\uC5EC\uC90D\
  \uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.554733-06:00'
model: gpt-4-1106-preview
summary: "\uB450 \uC22B\uC790\uC758 \uD569\uC744 \uACC4\uC0B0\uD558\uB294 \uD568\uC218\
  \uB97C \uC791\uC131\uD574 \uBCF4\uACA0\uC2B5\uB2C8\uB2E4."
title: "\uCF54\uB4DC\uB97C \uD568\uC218\uB85C \uAD6C\uC131\uD558\uAE30"
weight: 18
---

## 방법:
두 숫자의 합을 계산하는 함수를 작성해 보겠습니다. 간단하지만, 중요한 점을 잘 보여줍니다.

```PowerShell
function Add-Numbers {
    param (
        [int]$FirstNum,
        [int]$SecondNum
    )
    return $FirstNum + $SecondNum
}

# 5와 10으로 함수를 호출합니다
$sum = Add-Numbers -FirstNum 5 -SecondNum 10
Write-Output "합계는 $sum 입니다"
```

샘플 출력:

```
합계는 15 입니다
```

## 심층 분석
PowerShell에서의 함수는 대부분의 언어와 마찬가지로 새로운 것이 아닙니다. Fortran 시대부터 우리는 코드를 여러 개로 나누어 왔습니다. '바퀴를 다시 발명하지 않는 것'에 관한 것입니다. 대안이 있나요? 물론, 스크립트나 cmdlet이 있습니다. 그러나 이들은 스크립트 내의 함수가 갖는 정돈성과 상황에 맞는 유연성을 가지고 있지 않습니다.

구현 방법은요? 함수는 우리의 예제처럼 간단할 수도 있고, 범위, 파이프라인 입력 등을 포함한 복잡한 기능을 가질 수도 있습니다. `Advanced Functions`을 살펴보십시오. 이 함수들은 `[Parameter(Mandatory=$true)]`와 같은 속성을 가진 매개변수와 함께 cmdlet을 모방합니다. 이것은 PowerShell의 유연성을 맛보는 것입니다.

## 참고:
- [about_Functions_Advanced_Parameters](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_functions_advanced_parameters?view=powershell-7.1)
- [about_Script_Blocks](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_script_blocks?view=powershell-7.1)
