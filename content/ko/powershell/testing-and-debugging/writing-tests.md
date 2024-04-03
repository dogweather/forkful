---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:31:52.584504-07:00
description: "PowerShell\uC5D0\uC11C \uD14C\uC2A4\uD2B8\uB97C \uC791\uC131\uD55C\uB2E4\
  \uB294 \uAC83\uC740 PowerShell \uCF54\uB4DC\uC758 \uAE30\uB2A5\uC744 \uC790\uB3D9\
  \uC73C\uB85C \uAC80\uC99D\uD558\uB294 \uC2A4\uD06C\uB9BD\uD2B8\uB97C \uB9CC\uB4DC\
  \uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4. \uC774\uB807\uAC8C \uD558\uC5EC\
  \ \uCF54\uB4DC\uAC00 \uC608\uC0C1\uB300\uB85C \uB3D9\uC791\uD558\uB294\uC9C0 \uD655\
  \uC778\uD569\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC774\uB97C\
  \ \uD1B5\uD574 \uBC84\uADF8\uB97C \uC870\uAE30\uC5D0 \uBC1C\uACAC\uD558\uACE0, \uCF54\
  \uB4DC \uC720\uC9C0 \uAD00\uB9AC\uB97C \uB2E8\uC21C\uD654\uD558\uBA70, \uCF54\uB4DC\
  \u2026"
lastmod: '2024-03-13T22:44:55.551565-06:00'
model: gpt-4-0125-preview
summary: "PowerShell\uC5D0\uC11C \uD14C\uC2A4\uD2B8\uB97C \uC791\uC131\uD55C\uB2E4\
  \uB294 \uAC83\uC740 PowerShell \uCF54\uB4DC\uC758 \uAE30\uB2A5\uC744 \uC790\uB3D9\
  \uC73C\uB85C \uAC80\uC99D\uD558\uB294 \uC2A4\uD06C\uB9BD\uD2B8\uB97C \uB9CC\uB4DC\
  \uB294 \uAC83\uC744 \uC758\uBBF8\uD569\uB2C8\uB2E4."
title: "\uD14C\uC2A4\uD2B8 \uC791\uC131\uD558\uAE30"
weight: 36
---

## 방법:
PowerShell에는 내장된 테스팅 프레임워크가 없지만, 많은 사람들이 사용하는 인기 있는 서드파티 모듈인 Pester를 사용하여 테스트를 작성하고 실행합니다. 여기에서는 Pester를 사용하여 PowerShell 함수를 테스트하는 방법을 소개합니다.

먼저, 아직 Pester를 설치하지 않았다면 설치하세요:

```powershell
Install-Module -Name Pester -Scope CurrentUser -Force
```

다음으로, 테스트하고 싶은 간단한 PowerShell 함수가 `MyFunction.ps1`로 저장되어 있다고 가정합니다:

```powershell
function Get-MultipliedNumber {
    param (
        [int]$Number,
        [int]$Multiplier = 2
    )

    return $Number * $Multiplier
}
```

이 함수를 Pester로 테스트하려면, `MyFunction.Tests.ps1`이라는 테스트 스크립트를 생성합니다. 이 스크립트에서 Pester의 `Describe` 및 `It` 블록을 사용하여 테스트 케이스를 정의하세요:

```powershell
# 테스트할 함수를 임포트
. .\MyFunction.ps1

Describe "Get-MultipliedNumber tests" {
    It "곱셈기가 제공되지 않을 때 숫자를 2배로 곱한다" {
        $result = Get-MultipliedNumber -Number 3
        $result | Should -Be 6
    }

    It "주어진 곱셈기로 숫자를 올바르게 곱한다" {
        $result = Get-MultipliedNumber -Number 3 -Multiplier 3
        $result | Should -Be 9
    }
}
```

테스트를 실행하려면, PowerShell을 열고 테스트 스크립트가 있는 디렉토리로 이동한 다음, `Invoke-Pester` 명령을 사용하세요:

```powershell
Invoke-Pester .\MyFunction.Tests.ps1
```

샘플 출력은 다음과 같으며, 테스트가 통과했는지 실패했는지를 나타냅니다:

```
1개 파일에서 발견 시작.
발견 완료: 152ms.
[+] C:\path\to\MyFunction.Tests.ps1 204ms (182ms|16ms)
테스트 완료: 204ms
테스트 통과: 2, 실패: 0, 건너뛴 것: 0 실행되지 않음: 0
```

이 출력은 두 테스트 모두 통과했음을 보여주며, 테스트한 시나리오 하에서 `Get-MultipliedNumber` 함수가 예상대로 동작한다는 확신을 줍니다.
