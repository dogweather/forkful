---
date: 2024-01-20 17:33:35.702869-07:00
description: "How to: (\uBC29\uBC95:) PowerShell\uC5D0\uC11C \uB0A0\uC9DC \uBE44\uAD50\
  \uB294 `Get-Date`, `-eq`, `-gt`, `-lt` \uC640 \uAC19\uC740 Cmdlet\uACFC \uC5F0\uC0B0\
  \uC790\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4. \uC774 \uAE30\uB2A5\uC740 .NET\uC758\
  \ DateTime \uAC1D\uCCB4\uB97C \uAE30\uBC18\uC73C\uB85C \uD569\uB2C8\uB2E4. \uCD08\
  \uAE30 \uBC84\uC804\uC758 PowerShell\uC5D0\uB294 \uC774\uB807\uAC8C \uAC04\uACB0\
  \uD558\uACE0\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:57.217151-06:00'
model: gpt-4-1106-preview
summary: "(\uBC29\uBC95:) PowerShell\uC5D0\uC11C \uB0A0\uC9DC \uBE44\uAD50\uB294 `Get-Date`,\
  \ `-eq`, `-gt`, `-lt` \uC640 \uAC19\uC740 Cmdlet\uACFC \uC5F0\uC0B0\uC790\uB97C\
  \ \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uB450 \uB0A0\uC9DC \uBE44\uAD50\uD558\uAE30"
weight: 27
---

## How to: (방법:)
```PowerShell
# 예제 1: 날짜 비교 - 같음
$date1 = Get-Date '2023-03-15'
$date2 = Get-Date '2023-03-15'
$date1 -eq $date2  # 출력: True

# 예제 2: 날짜 비교 - 더 이전
$date3 = Get-Date '2021-06-10'
$date1 -gt $date3  # 출력: True

# 예제 3: 날짜 차이 계산
$diff = $date1 - $date3
$diff.Days  # 출력: 644
```

## Deep Dive (심층 탐구)
PowerShell에서 날짜 비교는 `Get-Date`, `-eq`, `-gt`, `-lt` 와 같은 Cmdlet과 연산자를 사용합니다. 이 기능은 .NET의 DateTime 객체를 기반으로 합니다. 초기 버전의 PowerShell에는 이렇게 간결하고 직관적인 방식이 없었지만, 버전이 업데이트되면서 개선되었습니다.

다른 방법으로 `[datetime]` 형 변환을 사용한 비교 또는 `.Compare()` 메소드를 사용할 수도 있습니다. `.Compare()`는 두 날짜를 비교하여 전자가 후자보다 이전이면 음수를, 같으면 0을, 늦으면 양수를 반환합니다.

비교를 구현할 때는 시간대를 고려해야 할 수 있습니다. 날짜가 서로 다른 시간대에서 온 것이라면, UTC로 변환하여 비교하는 것이 정확합니다.

## See Also (더 보기)
- [DateTime.Compare 메소드](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compare?view=netframework-4.8)
- [PowerShell about_Comparison_Operators](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7.1)
