---
title:                "난수 생성하기"
date:                  2024-01-20T17:49:37.478385-07:00
model:                 gpt-4-1106-preview
simple_title:         "난수 생성하기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
랜덤 숫자 생성은 예측할 수 없는 숫자 시퀀스를 만드는 것입니다. 프로그래머들은 종종 테스트 데이터, 게임, 보안 및 시뮬레이션 등의 목적으로 이를 사용합니다.

## How to (방법)
PowerShell에서 랜덤 숫자를 생성하려면 `Get-Random` cmdlet을 사용합니다. 예시를 보여드릴게요:

```PowerShell
# 1부터 100까지의 랜덤한 숫자를 생성합니다.
$randomNumber = Get-Random -Minimum 1 -Maximum 101
Write-Host "생성된 랜덤 숫자: $randomNumber"
```

```
생성된 랜덤 숫자: 42
```

범위 없이 숫자를 생성하려면:

```PowerShell
# 어떤 범위도 정하지 않고 랜덤한 숫자를 생성합니다.
$randomNumber = Get-Random
Write-Host "생성된 랜덤 숫자: $randomNumber"
```

## Deep Dive (심층 분석)
랜덤 숫자 생성은 컴퓨터과학에서 오래된 문제 중 하나입니다. 완벽한 랜덤은 컴퓨터로는 생성하기 어렵기 때문에 대부분의 랜덤함수들은 의사난수(pseudorandom)를 생성합니다. `Get-Random`은 .NET의 `System.Random` 클래스를 기반으로 합니다.

대안으로, 암호학적으로 안전한 랜덤 숫자가 필요할 때는 `System.Security.Cryptography.RNGCryptoServiceProvider`를 사용할 수 있습니다.

```PowerShell
# 암호학적으로 안전한 랜덤 바이트 생성
$bytes = New-Object byte[] 10
$rng = [System.Security.Cryptography.RandomNumberGenerator]::Create()
$rng.GetBytes($bytes)
$bytes
```

`System.Random`과 달리 `RNGCryptoServiceProvider`는 예측할 수 없는 데이터를 생성합니다, 하지만 대체적으로 사용하기 복잡하고 속도가 느립니다.

## See Also (더보기)
- PowerShell의 공식 문서: [Get-Random](https://docs.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Utility/Get-Random)
- .NET의 `System.Random` 클래스: [System.Random Class](https://docs.microsoft.com/en-us/dotnet/api/system.random)
- 암호학적 난수 생성기: [RNGCryptoServiceProvider Class](https://docs.microsoft.com/en-us/dotnet/api/system.security.cryptography.rngcryptoserviceprovider)
