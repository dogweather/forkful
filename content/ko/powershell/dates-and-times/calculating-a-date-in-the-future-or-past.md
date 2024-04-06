---
date: 2024-01-20 17:32:00.688872-07:00
description: "How to: (\uBC29\uBC95) PowerShell\uC5D0\uC11C \uB0A0\uC9DC \uACC4\uC0B0\
  \uC740 .NET\uC758 DateTime \uAC1D\uCCB4\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC774\uB8E8\
  \uC5B4\uC9D1\uB2C8\uB2E4. \uAC04\uB2E8\uD55C .AddDays() \uBA54\uC11C\uB4DC \uD638\
  \uCD9C\uB85C \uB0A0\uC9DC\uB97C \uCD94\uAC00\uD558\uAC70\uB098 \uBE7C\uB294 \uAC83\
  \uC774 \uAC00\uB2A5\uD569\uB2C8\uB2E4. \uC774 \uBC29\uBC95\uC740 \uC608\uC815\uB41C\
  \ \uC791\uC5C5\uC744 \uACC4\uD68D\uD558\uAC70\uB098 \uB85C\uADF8 \uD30C\uC77C\uC744\
  \ \uCC98\uB9AC\uD560 \uB54C \uC720\uC6A9\uD569\uB2C8\uB2E4.\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.218267-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95) PowerShell\uC5D0\uC11C \uB0A0\uC9DC \uACC4\uC0B0\uC740 .NET\uC758\
  \ DateTime \uAC1D\uCCB4\uB97C \uC0AC\uC6A9\uD558\uC5EC \uC774\uB8E8\uC5B4\uC9D1\uB2C8\
  \uB2E4."
title: "\uBBF8\uB798\uB098 \uACFC\uAC70\uC758 \uB0A0\uC9DC \uACC4\uC0B0\uD558\uAE30"
weight: 26
---

## How to: (방법)
```PowerShell
# 미래 날짜 계산하기
$daysToAdd = 30
$futureDate = (Get-Date).AddDays($daysToAdd)
Write-Output "30일 후 날짜: $futureDate"

# 출력 예시: 30일 후 날짜: Friday, April 14, 2023 5:52:48 PM

# 과거 날짜 계산하기
$daysToSubtract = 10
$pastDate = (Get-Date).AddDays(-$daysToSubtract)
Write-Output "10일 전 날짜: $pastDate"

# 출력 예시: 10일 전 날짜: Tuesday, March 21, 2023 5:52:48 PM
```

## Deep Dive (깊이 있는 정보)
PowerShell에서 날짜 계산은 .NET의 DateTime 객체를 사용하여 이루어집니다. 간단한 .AddDays() 메서드 호출로 날짜를 추가하거나 빼는 것이 가능합니다. 이 방법은 예정된 작업을 계획하거나 로그 파일을 처리할 때 유용합니다.

AddDays() 외에도 .AddHours(), .AddMinutes(), .AddMonths() 등 다양한 메서드가 있어 세밀한 날짜 조정이 가능합니다. PowerShell 1.0부터 사용 가능했으며, 후에 나온 버전들에서 기능이 더해지고 성능이 개선되었습니다.

.NET 대안으로 DateTimeOffset이 있지만, 특정 시간대와 관련된 계산이 필요할 때만 사용됩니다. PowerShell 5.1 이후 버전에서는 "New-TimeSpan" cmdlet을 사용하여 날짜 범위를 계산할 수도 있습니다.

내장된 날짜 계산 기능은 강력하며, 대부분의 경우 별도의 모듈을 설치할 필요가 없습니다. 그러나 복잡한 날짜 계산은 때때로 추가적인 라이브러리나 모듈의 도움을 필요로 할 수 있습니다.

## See Also (더 보기)
- 공식 PowerShell 문서의 날짜와 시간에 관한 섹션:
  [PowerShell Datetime](https://docs.microsoft.com/en-us/powershell/scripting/learn/deep-dives/everything-about-datetime?view=powershell-7.2)

- .NET DateTime 클래스:
  [.NET DateTime](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)

- PowerShell Gallery에서 날짜 관련 모듈 검색하기:
  [PowerShell Gallery](https://www.powershellgallery.com/)
