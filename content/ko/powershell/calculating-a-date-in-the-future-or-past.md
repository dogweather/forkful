---
title:                "미래나 과거의 날짜 계산하기"
date:                  2024-01-20T17:32:00.688872-07:00
model:                 gpt-4-1106-preview
simple_title:         "미래나 과거의 날짜 계산하기"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
날짜 계산은 미래나 과거의 특정 날짜를 구하는 것입니다. 스케줄링, 만기일 확인, 이벤트 예정일 설정 등을 위해 프로그래머들이 이를 수행합니다.

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