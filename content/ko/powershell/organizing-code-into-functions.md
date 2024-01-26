---
title:                "코드를 함수로 구성하기"
date:                  2024-01-26T01:11:20.007326-07:00
model:                 gpt-4-1106-preview
simple_title:         "코드를 함수로 구성하기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## 무엇이며 왜 사용하는가?
코드를 함수로 구성하는 것은 특정 작업을 수행하는 코드 덩어리를 감싸고 이름을 지어주는 것입니다. 이것은 코드를 재사용 가능하고, 읽기 쉽고, 유지 보수하기 쉽도록 하기 위해 수행됩니다. 동일한 코드를 다시 작성하는 대신 함수를 호출하십시오. 문제를 해결하거나 업그레이드하고 싶으신가요? 스크립트 더미를 뒤져볼 필요 없이 함수만 조정하면 됩니다.

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