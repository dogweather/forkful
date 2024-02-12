---
title:                "난수 생성"
aliases:
- ko/powershell/generating-random-numbers.md
date:                  2024-01-27T20:35:07.573051-07:00
model:                 gpt-4-0125-preview
simple_title:         "난수 생성"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
PowerShell에서 무작위 수를 생성하는 것은 지정된 범위 내에서 예측할 수 없는 숫자 값을 만드는 것입니다. 프로그래머들은 예측 불가능성이 중요하거나 실제 세계의 무작위성을 모방하는 것과 같이 테스트, 시뮬레이션 및 보안 목적을 포함하여 다양한 이유로 이 기능을 활용합니다.

## 방법:
PowerShell은 `Get-Random` cmdlet을 사용하여 무작위 수를 생성하는 간단한 접근 방식을 제공합니다. 이 cmdlet은 기본 범위 또는 지정된 범위 내에서 무작위 수를 생성할 수 있습니다.

```PowerShell
# 0과 Int32.MaxValue 사이의 무작위 수 생성
$randomNumber = Get-Random
Write-Output $randomNumber
```

범위를 지정하려면 `-Minimum` 및 `-Maximum` 매개변수를 사용하세요:

```PowerShell
# 1과 100 사이의 무작위 수 생성
$randomNumber = Get-Random -Minimum 1 -Maximum 101
Write-Output $randomNumber
```

더 많은 제어가 필요하다면 `System.Random` 클래스의 객체를 인스턴스화할 수 있습니다:

```PowerShell
# System.Random 사용하여 숫자 시퀀스 생성
$rand = New-Object System.Random
foreach ($i in 1..5) {
    $randomNumber = $rand.Next(1, 101)
    Write-Output $randomNumber
}
```

배열이나 컬렉션에서 임의의 선택이 필요한 경우, `Get-Random`은 직접 항목을 선택할 수 있습니다:

```PowerShell
# 배열에서 무작위 선택
$array = 1..10
$randomItem = Get-Random -InputObject $array
Write-Output $randomItem
```

## 심층 탐구
PowerShell의 `Get-Random` cmdlet은 내부적으로 .NET 클래스 `System.Random`을 사용하여 의사(pseudo) 무작위 수를 생성합니다. 알고리즘을 사용하여 임의로 보이는 숫자 시퀀스를 생성하기 때문에 "pseudo"라고 불립니다. 대부분의 응용 프로그램에는 이 수준의 무작위성이 충분합니다. 하지만, 암호학적 보안이 필요한 사용 사례의 경우, 예측 가능한 성질로 인해 `System.Random`은 적합하지 않습니다.

암호학적 무작위성이 필요한 경우, PowerShell과 .NET은 암호화 키 생성이나 기타 보안에 민감한 작업에 더 적합한 `System.Security.Cryptography.RNGCryptoServiceProvider`를 제공합니다:

```PowerShell
# 암호학적으로 안전한 무작위 수
$rng = [System.Security.Cryptography.RNGCryptoServiceProvider]::new()
$bytes = New-Object byte[] 4
$rng.GetBytes($bytes)
$randomNumber = [BitConverter]::ToInt32($bytes, 0)
Write-Output $randomNumber
```

`Get-Random` 및 `System.Random`은 스크립팅 및 애플리케이션 로직에서 필요한 무작위성에 대한 광범위한 요구사항을 만족시키지만, 특히 예측 가능성이 취약점을 제시할 수 있는 보안 중심의 애플리케이션에서는 적합한 도구를 선택하는 것이 중요합니다.
